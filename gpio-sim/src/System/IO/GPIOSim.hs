module System.IO.GPIOSim where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict
import Data.Foldable
import Data.Function ((&))
import Data.Functor.Identity
import Data.Map.Strict (Map)
import Data.Sequence (Seq(..))
import Data.Text (Text)
import GHC.Exts
import System.Directory
import System.FilePath
import System.Random
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

data DeviceSpec = DeviceSpec
  { banks :: [Bank]
  } deriving (Show, Eq, Ord)

instance Semigroup DeviceSpec where
  DeviceSpec b1 <> DeviceSpec b2 =
    DeviceSpec (b1 <> b2)

instance Monoid DeviceSpec where
  mempty = DeviceSpec mempty
  mappend = (<>)

newDeviceSpec :: DeviceSpec
newDeviceSpec = mempty

addBank :: Bank -> DeviceSpec -> DeviceSpec
addBank b (DeviceSpec bs) = DeviceSpec (b : bs)

data Bank = Bank
  { lineCount :: Word
  , lineNames :: Map LineOffset LineName
  , lineHogs :: Map LineOffset HogMode
  } deriving (Show, Eq, Ord)

newBank :: Word -> Bank
newBank w = Bank
  { lineCount = w
  , lineNames = mempty
  , lineHogs = mempty
  }

newtype LineOffset = LineOffset Word
  deriving (Show, Eq, Ord)

newtype LineName = LineName Text
  deriving (Show, Eq, Ord, IsString)

nameLine :: Word -> LineName -> Bank -> Bank
nameLine w ln (Bank c ns hs) =
  Bank c (Map.insert (LineOffset w) ln ns) hs

data HogMode = HogMode
  { consumer :: ConsumerName
  , direction :: Direction
  } deriving (Show, Eq, Ord)

hogLine :: Word -> ConsumerName -> Direction -> Bank -> Bank
hogLine w cn d (Bank c ns hs) =
  Bank c ns (Map.insert (LineOffset w) (HogMode cn d) hs)

newtype ConsumerName = ConsumerName Text
  deriving (Show, Eq, Ord, IsString)

data Direction
  = Input
  | OutputHigh
  | OutputLow
  deriving (Show, Eq, Ord, Enum, Bounded)

directionText :: Direction -> Text
directionText = \case
  Input -> "input"
  OutputHigh -> "output-high"
  OutputLow -> "output-low"

data GPIOSimOptions = GPIOSimOptions
  { gpioSimDirectoryPath :: FilePath
  , dryRun :: Bool
  , deviceDirectoryName :: Maybe FilePath
  } deriving (Show, Eq, Ord)

defaultGPIOSimOptions :: GPIOSimOptions
defaultGPIOSimOptions = GPIOSimOptions
  { gpioSimDirectoryPath = "/sys/kernel/config/gpio-sim"
  , dryRun = False
  , deviceDirectoryName = Nothing
  }

newtype SimM m a = SimM (WriterT (Seq (FileAction FilePath)) m a)
  deriving (Functor, Applicative, Monad, MonadIO)

runSimM :: SimM m a -> m (a, Seq (FileAction FilePath))
runSimM (SimM act) = runWriterT act

data FileAction a
  = CreateDirectory a
  | WriteText a Text
  | WriteWord a Word
  | WriteDirection a Direction
  | WriteBool a Bool
  | RemoveDirectory a
  deriving (Show, Eq, Ord, Functor)

addBaseDirectory :: FilePath
                 -> FileAction FilePath
                 -> FileAction FilePath
addBaseDirectory dir = fmap (dir </>)

displayFileAction :: FileAction FilePath -> Text
displayFileAction = \case
  CreateDirectory fp ->
    "create directory: \n  " <> Text.pack fp
  WriteText fp t ->
    "write " <> Text.pack (show t) <> " to file: \n  " <> Text.pack fp
  WriteWord fp w ->
    "write " <> Text.pack (show w) <> " to file: \n  " <> Text.pack fp
  WriteDirection fp d ->
    "write " <> Text.pack (show (directionText d)) <> " to file: \n  " <> Text.pack fp
  WriteBool fp b ->
    "write " <> (if b then "\"1\"" else "\"0\"") <> " to file: \n  " <> Text.pack fp
  RemoveDirectory fp ->
    "remove directory: \n  " <> Text.pack fp

executeFileAction :: FilePath -> FileAction FilePath -> IO ()
executeFileAction baseDir act =
  case addBaseDirectory baseDir act of
    CreateDirectory fp -> createDirectory fp
    WriteText fp t -> Text.IO.writeFile fp t
    WriteWord fp w -> Text.IO.writeFile fp (Text.pack (show w))
    WriteDirection fp d -> Text.IO.writeFile fp (directionText d)
    WriteBool fp b -> Text.IO.writeFile fp (if b then "1" else "0")
    RemoveDirectory fp -> removeDirectory fp

tellCreateDirectory :: Monad m => FilePath -> SimM m ()
tellCreateDirectory fp = SimM $ tell $ pure $ CreateDirectory fp

tellWriteText :: Monad m => FilePath -> Text -> SimM m ()
tellWriteText fp t = SimM $ tell $ pure $ WriteText fp t

tellWriteWord :: Monad m => FilePath -> Word -> SimM m ()
tellWriteWord fp w = SimM $ tell $ pure $ WriteWord fp w

tellWriteDirection :: Monad m => FilePath -> Direction -> SimM m ()
tellWriteDirection fp d = SimM $ tell $ pure $ WriteDirection fp d

tellWriteBool :: Monad m => FilePath -> Bool -> SimM m ()
tellWriteBool fp b = SimM $ tell $ pure $ WriteBool fp b

tellRemoveDirectory :: Monad m => FilePath -> SimM m ()
tellRemoveDirectory fp = SimM $ tell $ pure $ RemoveDirectory fp

invert :: FileAction a -> Maybe (FileAction a)
invert = \case
  CreateDirectory fp -> Just $ RemoveDirectory fp
  WriteBool fp b -> Just $ WriteBool fp (not b)
  _ -> Nothing

reverseActionSequence :: Seq (FileAction a) -> Seq (FileAction a)
reverseActionSequence =
  foldl' (\s fa -> maybe s (:<| s) (invert fa)) mempty

mkGPIOSimDirectoryName :: IO FilePath
mkGPIOSimDirectoryName = do
  (w, _) <- genWord64 <$> initStdGen
  pure $ "gpio-sim-" <> show w

loadDeviceSpec :: GPIOSimOptions -> DeviceSpec -> IO ()
loadDeviceSpec GPIOSimOptions{..} spec = do
  dir <- maybe mkGPIOSimDirectoryName pure deviceDirectoryName

  let actions = generateActionSequence dir spec

  if dryRun
    then do
      printDryRunPlannedActions actions
    else do
      pure ()

printDryRunPlannedActions :: Seq (FileAction FilePath) -> IO ()
printDryRunPlannedActions actions = do
  let actionsReversed = reverseActionSequence actions

  Text.IO.putStrLn $ Text.unlines
    [ "\n"
    , Text.replicate 80 "!"
    , "this is a dry run (dryRun), and no actions will be taken."
    , "here are the actions I will take in a non-dry run for this DeviceSpec:"
    , Text.replicate 80 "!"
    , "\n"

    , "to create the simulated device, I will perform the following actions:"
    , Text.replace "\n" "\n  " $ describeActionSequence actions
    , "\n"

    , "then, to remove the device, I will perform the following actions:"
    , Text.replace "\n" "\n  " $ describeActionSequence actionsReversed
    ]

withDeviceInfo :: GPIOSimOptions
               -> DeviceSpec
               -> (DeviceInfo -> IO a)
               -> IO a
withDeviceInfo GPIOSimOptions{..} spec act = do
  dir <- maybe mkGPIOSimDirectoryName pure deviceDirectoryName

  let actions = generateActionSequence dir spec

  if dryRun
    then do
      printDryRunPlannedActions actions
      error "withDeviceSpec: cannot continue when dryRun is enabled"
    else do
      go actions $ do
        di <- getDeviceInfo (gpioSimDirectoryPath </> dir) spec
        act di
  where
    go Seq.Empty f = f
    go (x :<| xs) f = do
      bracket_
        (executeFileAction gpioSimDirectoryPath x)
        (maybe (pure ()) (executeFileAction gpioSimDirectoryPath) (invert x))
        (go xs f)

data DeviceInfo = DeviceInfo
  { devName :: Text
  , bankInfos :: [BankInfo]
  , live :: Bool
  } deriving (Show, Eq, Ord)

data BankInfo = BankInfo
  { chipName :: Text
  , bankSpec :: Bank
  } deriving (Show, Eq, Ord)

getDeviceInfo :: FilePath -> DeviceSpec -> IO DeviceInfo
getDeviceInfo deviceDir DeviceSpec{..} = do
  let readFileChomp fp =
        Text.dropWhileEnd (== '\n') <$> Text.IO.readFile fp

  devName <- readFileChomp $ devNameFilePath deviceDir

  bankInfos <- forM (zip banks [0..]) $ \(bankSpec, bankIndex) -> do
    chipName <- readFileChomp $ bankChipNameFilePath deviceDir bankIndex
    pure BankInfo{..}

  live <- readFileChomp (liveFilePath deviceDir) >>= \case
    "1" -> pure True
    "0" -> pure False
    other -> error $ "Unable to access liveness state: expected /live to contain either \"0\" or \"1\", but got " <> show other

  pure DeviceInfo{..}

generateActionSequence :: FilePath
                       -> DeviceSpec
                       -> Seq (FileAction FilePath)
generateActionSequence dir DeviceSpec{..} =
  snd $ runIdentity $ runSimM $ do
    -- create the top-level directory for the device
    tellCreateDirectory dir

    -- for each bank in the spec
    forM_ (zip banks [0..]) $ \(Bank{..}, bankIndex) -> do

      -- create a directory for the bank
      tellCreateDirectory (bankFilePath dir bankIndex)

      -- create a directory for the bank
      tellWriteWord (bankLineCountFilePath dir bankIndex) lineCount

      -- for each line in the spec
      forM_ [0.. pred lineCount] $ \lineIndex -> do
        let offset = LineOffset lineIndex

        -- create the line
        tellCreateDirectory $ lineFilePath dir bankIndex offset

        forM_ (Map.lookup offset lineNames) $ \(LineName ln) ->
          -- write the line name (if there is one)
          tellWriteText (lineNameFilePath dir bankIndex offset) ln

        forM_ (Map.lookup offset lineHogs) $ \HogMode{..} -> do
          let ConsumerName consumerText = consumer
          -- create the hog directory (if there is one)
          tellCreateDirectory (lineHogFilePath dir bankIndex offset)
          -- write the consumer name
          tellWriteText (lineHogConsumerFilePath dir bankIndex offset) consumerText
          -- write the hog direction
          tellWriteDirection (lineHogDirectionFilePath dir bankIndex offset) direction

    -- write the live file
    tellWriteBool (liveFilePath dir) True

describeActionSequence :: Seq (FileAction FilePath) -> Text
describeActionSequence actions = Text.unlines
  [ intercalate "\n" (fmap displayFileAction actions)
  ]

intercalate :: (Foldable f, Monoid m) => m -> f m -> m
intercalate sep xs = foldl' (\a b -> a <> sep <> b) mempty xs

devNameFilePath :: FilePath -> FilePath
devNameFilePath dir = dir </> "dev_name"

liveFilePath :: FilePath -> FilePath
liveFilePath dir = dir </> "live"

bankFilePath :: FilePath -> Word -> FilePath
bankFilePath dir n = dir </> "gpio-bank" <> show n

bankLineCountFilePath :: FilePath -> Word -> FilePath
bankLineCountFilePath dir n =
  dir </> "gpio-bank" <> show n </> "num_lines"

bankChipNameFilePath :: FilePath -> Word -> FilePath
bankChipNameFilePath dir n =
  dir </> "gpio-bank" <> show n </> "chip_name"

lineFilePath :: FilePath -> Word -> LineOffset -> FilePath
lineFilePath dir n (LineOffset o) =
  bankFilePath dir n </> "line" <> show o

lineNameFilePath :: FilePath -> Word -> LineOffset -> FilePath
lineNameFilePath dir n o =
  lineFilePath dir n o </> "name"

lineHogFilePath :: FilePath -> Word -> LineOffset -> FilePath
lineHogFilePath dir n o =
  lineFilePath dir n o </> "hog"

lineHogConsumerFilePath :: FilePath -> Word -> LineOffset -> FilePath
lineHogConsumerFilePath dir n o =
  lineHogFilePath dir n o </> "name"

lineHogDirectionFilePath :: FilePath -> Word -> LineOffset -> FilePath
lineHogDirectionFilePath dir n o =
  lineHogFilePath dir n o </> "direction"

exampleDeviceSpec :: DeviceSpec
exampleDeviceSpec =
  newDeviceSpec
    & addBank exampleBank
  where
    exampleBank =
      newBank 16
        & nameLine 0 "albert"
        & hogLine 0 "albert-hog" OutputLow
        & nameLine 1 "brenda"
        & hogLine 1 "brenda-hog" OutputHigh
        & nameLine 2 "christine"
        & hogLine 2 "christine-hog" Input
        & nameLine 3 "daphne"
