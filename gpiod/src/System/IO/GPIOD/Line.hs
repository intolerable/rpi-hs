module System.IO.GPIOD.Line
  ( Line()
  , lookupChipLineByNumber
  , lookupChipLineByName
  , lookupLineByName
  , LineInfo(..)
  , getLineInfo
  , getLineName
  , getLineConsumer
  , Direction(..)
  , getLineDirection
  , ActiveState(..)
  , getLineActiveState
  , Bias(..)
  , getLineBias
  , getLineUsed
  , getLineOpenDrain
  , getLineOpenSource
  , getLineOffset
  , getLineValue
  , allChipLines
  , withLinePtr
  ) where

import Data.Bits (testBit)
import Data.Bifunctor
import Control.Exception
import Data.Functor
import Data.Text (Text)
import Foreign.C.ConstPtr
import Foreign.ForeignPtr
import Foreign.Ptr
import Streaming (Stream, Of, MonadIO(..))
import qualified Data.Text.Foreign as Text.Foreign
import qualified Streaming.Prelude as S

import qualified System.IO.LibGPIOD as LibGPIOD
import System.IO.GPIOD.Chip.Internal (Chip, withChipPtr, unsafeMkChip, ChipOwned, mkChipOwned, withChipOwned)
import System.IO.GPIOD.Error

newtype Line = Line (ChipOwned (Ptr LibGPIOD.GPIODLine))

withLinePtr :: Line -> (Ptr LibGPIOD.GPIODLine -> IO a) -> IO a
withLinePtr (Line c) f = withChipOwned c $ \_cptr lptr -> f lptr

mkLine :: Text
       -> Chip
       -> IO (Ptr LibGPIOD.GPIODLine)
       -> IO (Either (GPIODError '[]) (Maybe Line))
mkLine l chip f = do
  r <- fromPtrIO l f pure
  case first unshiftGPIODError r of
    Left (Left err) -> pure $ Left err
    Left (Right LineDoesNotExist) -> pure $ Right Nothing
    Right lptr -> pure $ Right $ Just $ Line $ mkChipOwned chip lptr

lookupChipLineByNumber :: Chip -> Word -> IO (Either (GPIODError '[]) (Maybe Line))
lookupChipLineByNumber c i = withChipPtr c $ \cptr ->
  mkLine "chipGetLine" c (LibGPIOD.gpiod_chip_get_line cptr (fromIntegral i))

lookupChipLineByName :: Chip -> Text -> IO (Either (GPIODError '[]) (Maybe Line))
lookupChipLineByName c t = withChipPtr c $ \cptr ->
  Text.Foreign.withCString t $ \str ->
    mkLine "chipFindLine" c (LibGPIOD.gpiod_chip_find_line cptr (ConstPtr str))

lookupLineByName :: Text -> IO (Either (GPIODError '[]) (Maybe Line))
lookupLineByName t = Text.Foreign.withCString t $ \str -> do
  bracketOnError (LibGPIOD.gpiod_line_find (ConstPtr str)) LibGPIOD.gpiod_line_close_chip $ \ptr -> do
    res <- fromPtr "findLine" ptr $ do
      chip <- unsafeMkChip $ LibGPIOD.gpiod_line_get_chip ptr
      pure $ mkChipOwned chip ptr
    case first unshiftGPIODError res of
      Left (Left err) -> pure $ Left err
      Left (Right LineDoesNotExist) -> pure $ Right Nothing
      Right r -> pure $ Right $ Just $ Line r

data LineInfo = LineInfo
  { offset :: Word
  , name :: Maybe Text
  , consumer :: Maybe Text
  , direction :: Direction
  , activeState :: ActiveState
  , bias :: Bias
  , used :: Bool
  , openDrain :: Bool
  , openSource :: Bool
  } deriving (Show, Eq, Ord)

getLineInfo :: Line -> IO LineInfo
getLineInfo l =
  LineInfo <$> getLineOffset l
           <*> getLineName l
           <*> getLineConsumer l
           <*> getLineDirection l
           <*> getLineActiveState l
           <*> getLineBias l
           <*> getLineUsed l
           <*> getLineOpenDrain l
           <*> getLineOpenSource l

getLineOffset :: Line -> IO Word
getLineOffset l = withLinePtr l $ \lptr ->
  fromIntegral <$> LibGPIOD.gpiod_line_offset lptr

getLineName :: Line -> IO (Maybe Text)
getLineName l = withLinePtr l $ \lptr -> do
  ConstPtr ptr <- LibGPIOD.gpiod_line_name lptr
  if ptr == nullPtr
    then pure Nothing
    else Just <$> Text.Foreign.peekCString ptr

getLineConsumer :: Line -> IO (Maybe Text)
getLineConsumer l =  withLinePtr l $ \lptr -> do
  ConstPtr ptr <- LibGPIOD.gpiod_line_consumer lptr
  if ptr == nullPtr
    then pure Nothing
    else Just <$> Text.Foreign.peekCString ptr

data Direction
  = Input
  | Output
  deriving (Show, Eq, Ord, Enum, Bounded)

getLineDirection :: Line -> IO Direction
getLineDirection l = withLinePtr l $ \lptr ->
  LibGPIOD.gpiod_line_direction lptr <&> \case
    LibGPIOD.GPIODLineDirectionInput -> Input
    LibGPIOD.GPIODLineDirectionOutput -> Output

data ActiveState
  = High
  | Low
  deriving (Show, Eq, Ord, Enum, Bounded)

getLineActiveState :: Line -> IO ActiveState
getLineActiveState l = withLinePtr l $ \lptr ->
  LibGPIOD.gpiod_line_active_state lptr <&> \case
    LibGPIOD.GPIODLineActiveStateHigh -> High
    LibGPIOD.GPIODLineActiveStateLow -> Low

data Bias
  = PullUp
  | PullDown
  | Disable
  | AsIs
  deriving (Show, Eq, Ord, Enum, Bounded)

getLineBias :: Line -> IO Bias
getLineBias l = withLinePtr l $ \lptr ->
  LibGPIOD.gpiod_line_bias lptr <&> \case
    LibGPIOD.GPIODLineBiasPullUp -> PullUp
    LibGPIOD.GPIODLineBiasPullDown -> PullDown
    LibGPIOD.GPIODLineBiasDisable -> Disable
    LibGPIOD.GPIODLineBiasAsIs -> AsIs

getLineUsed :: Line -> IO Bool
getLineUsed l = withLinePtr l $ \lptr ->
  fmap (`testBit` 0) $ LibGPIOD.gpiod_line_is_used lptr

getLineOpenDrain :: Line -> IO Bool
getLineOpenDrain l = withLinePtr l $ \lptr ->
  fmap (`testBit` 0) $ LibGPIOD.gpiod_line_is_open_drain lptr

getLineOpenSource :: Line -> IO Bool
getLineOpenSource l = withLinePtr l $ \lptr ->
  fmap (`testBit` 0) $ LibGPIOD.gpiod_line_is_open_source lptr

getLineValue :: Line -> IO (Either (GPIODError '[LineOperationNotPermitted]) Bool)
getLineValue l = withLinePtr l $ \lptr -> do
  fromCIntIO "getLineValue" (LibGPIOD.gpiod_line_get_value lptr) $ \cint ->
    pure $ cint `testBit` 0

data LineIter = LineIter
  { _lineIterOwnerChip :: Chip
  , _lineIterForeignPtr :: ForeignPtr LibGPIOD.GPIODLineIter
  }

mkLineIter :: Chip -> IO LineIter
mkLineIter c = withChipPtr c $ \cptr ->
  bracketOnError (LibGPIOD.gpiod_line_iter_new cptr) LibGPIOD.gpiod_line_iter_free $ \ptr -> do
    fptr <- newForeignPtr LibGPIOD.gpiod_line_iter_free_p ptr
    pure $ LineIter c fptr

nextLineIter :: LineIter -> IO (Either (GPIODError '[]) (Maybe Line))
nextLineIter (LineIter c fptr) = do
  withForeignPtr fptr $ \ptr -> do
    mkLine "nextLineIter" c $ LibGPIOD.gpiod_line_iter_next ptr

allChipLines :: MonadIO m => Chip -> Stream (Of Line) m (Either (GPIODError '[]) ())
allChipLines c = do
  iter <- liftIO (mkLineIter c)
  go iter
  where
    go i = do
      liftIO (nextLineIter i) >>= \case
        Left err -> pure $ Left err
        Right Nothing -> pure $ Right ()
        Right (Just l) -> S.yield l >> go i
