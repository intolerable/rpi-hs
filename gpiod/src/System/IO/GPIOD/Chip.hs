module System.IO.GPIOD.Chip
  ( Chip
  , lookupChipByName
  , lookupChipByLabel
  , lookupChipByNumber
  , ChipInfo(..)
  , getChipInfo
  , allChips
  ) where

import Data.Text (Text)
import Foreign.C.ConstPtr
import Streaming (Stream, Of, MonadIO(..))
import qualified Data.Text.Foreign as Text.Foreign
import qualified Streaming.Prelude as S

import System.IO.GPIOD.Chip.Internal
import System.IO.GPIOD.Error
import qualified LibGPIOD

lookupChipByName :: Text -> IO (Either (GPIODError '[]) (Maybe Chip))
lookupChipByName t =
  Text.Foreign.withCString t $ \str ->
    mkChip "lookupChipByName" $ LibGPIOD.gpiod_chip_open_by_name (ConstPtr str)

lookupChipByLabel :: Text -> IO (Either (GPIODError '[]) (Maybe Chip))
lookupChipByLabel t =
  Text.Foreign.withCString t $ \str ->
    mkChip "lookupChipByLabel" $ LibGPIOD.gpiod_chip_open_by_label (ConstPtr str)

lookupChipByNumber :: Word -> IO (Either (GPIODError '[]) (Maybe Chip))
lookupChipByNumber n =
  mkChip "lookupChipByNumber" $ LibGPIOD.gpiod_chip_open_by_number n

data ChipInfo = ChipInfo
  { name :: Text
  , label :: Text
  , lineCount :: Word
  } deriving (Show, Eq, Ord)

getChipInfo :: Chip -> IO ChipInfo
getChipInfo c =
  ChipInfo <$> getChipName c
           <*> getChipLabel c
           <*> getChipLineCount c

getChipName :: Chip -> IO Text
getChipName c = withChipPtr c $ \cptr ->
  LibGPIOD.gpiod_chip_name cptr >>= Text.Foreign.peekCString . unConstPtr

getChipLabel :: Chip -> IO Text
getChipLabel c = withChipPtr c $ \cptr ->
  LibGPIOD.gpiod_chip_label cptr >>= Text.Foreign.peekCString . unConstPtr

getChipLineCount :: Chip -> IO Word
getChipLineCount c = withChipPtr c $ \cptr ->
  fromIntegral <$> LibGPIOD.gpiod_chip_num_lines cptr

allChips :: MonadIO m => Stream (Of Chip) m (Either (GPIODError '[]) ())
allChips = do
  iter <- liftIO mkChipIter
  go iter
  where
    go i = do
      liftIO (nextChipIter i) >>= \case
        Left e -> pure $ Left e
        Right Nothing -> pure $ Right ()
        Right (Just c) -> S.yield c >> go i

