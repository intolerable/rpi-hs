module System.IO.IOCtl.RawSpec where

import Foreign.Storable
import Foreign.C.Types
import Data.Bits
import Test.Hspec
import Numeric (showHex, showBin)

import System.IO.IOCtl.Raw

#include "sys/ioctl.h"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  it "calculates IOWR correctly" $ do
    IOWR 1 1 (fromIntegral (sizeOf (undefined :: CChar)))
      `shouldBe` #{const _IOWR(1, 1, char)}

  it "has sensible constants" $ do
    IOC_NRBITS `shouldBe` #{const _IOC_NRBITS}

  it "masks" $ do
    IOC_WRITE `shouldBe` 1
    IOC_READ `shouldBe` 2
    IOC_WRITEREAD `shouldBe` 3
