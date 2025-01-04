module System.IO.GPIOD.Chip.Internal
  ( Chip(..)
  , mkChip
  , unsafeMkChip
  , withChipPtr
  , ChipOwned()
  , mkChipOwned
  , mkChipOwnedIO
  , withChipOwned
  , ChipIter()
  , mkChipIter
  , nextChipIter
  ) where

import Data.Bifunctor
import Data.Text (Text)
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Exception

import System.IO.GPIOD.Error
import qualified LibGPIOD

newtype Chip = Chip (ForeignPtr LibGPIOD.GPIODChip)

withChipPtr :: Chip -> (Ptr LibGPIOD.GPIODChip -> IO a) -> IO a
withChipPtr (Chip fptr) f = withForeignPtr fptr f

data ChipOwned a = ChipOwned Chip a

mkChipOwned :: Chip -> a -> ChipOwned a
mkChipOwned = ChipOwned

mkChipOwnedIO :: Chip -> IO a -> IO (ChipOwned a)
mkChipOwnedIO c a = ChipOwned c <$> a

withChipOwned :: ChipOwned a -> (Ptr LibGPIOD.GPIODChip -> a -> IO b) -> IO b
withChipOwned (ChipOwned c a) f = withChipPtr c $ \cptr -> f cptr a

mkChip :: Text
       -> IO (Ptr LibGPIOD.GPIODChip)
       -> IO (Either (GPIODError '[]) (Maybe Chip))
mkChip l f = bracketOnError f (LibGPIOD.gpiod_chip_close) $ \ptr -> do
  r <- fromPtr l ptr $ do
    fptr <- newForeignPtr LibGPIOD.gpiod_chip_close_p ptr
    pure $ Chip fptr
  case first unshiftGPIODError r of
    Left (Left err) -> pure $ Left err
    Left (Right ChipDoesNotExist) -> pure $ Right Nothing
    Right c -> pure $ Right $ Just c

unsafeMkChip :: IO (Ptr LibGPIOD.GPIODChip) -> IO Chip
unsafeMkChip f = bracketOnError f (LibGPIOD.gpiod_chip_close) $ \ptr -> do
  Chip <$> newForeignPtr LibGPIOD.gpiod_chip_close_p ptr

newtype ChipIter =
  ChipIter (ForeignPtr LibGPIOD.GPIODChipIter)

mkChipIter :: IO ChipIter
mkChipIter = fmap ChipIter $
  bracketOnError LibGPIOD.gpiod_chip_iter_new LibGPIOD.gpiod_chip_iter_free $
    newForeignPtr LibGPIOD.gpiod_chip_iter_free_noclose_p

nextChipIter :: ChipIter -> IO (Either (GPIODError '[]) (Maybe Chip))
nextChipIter (ChipIter fptr) = do
  withForeignPtr fptr $ \ptr -> do
    mkChip "nextChipIter" $ LibGPIOD.gpiod_chip_iter_next_noclose ptr
