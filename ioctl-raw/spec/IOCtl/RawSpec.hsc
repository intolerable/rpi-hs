module IOCtl.RawSpec where

import Foreign.Storable
import Foreign.C.Types
import Data.Bits
import Test.Hspec
import Numeric (showHex, showBin)

import IOCtl.Raw

#include "sys/ioctl.h"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  it "calculates IOWR correctly" $ do
    (#{const _IOC_NRSHIFT}, #{const _IOC_TYPESHIFT}, #{const _IOC_SIZESHIFT}, #{const _IOC_DIRSHIFT}) `shouldBe` (IOC_NRSHIFT, IOC_TYPESHIFT, IOC_SIZESHIFT, IOC_DIRSHIFT)
    IOWR 1 1 (fromIntegral (sizeOf (undefined :: CChar)))
      `shouldBe` #{const _IOWR(1, 1, char)}

  it "has sensible constants" $ do
    IOC_NRBITS `shouldBe` #{const _IOC_NRBITS}

  it "makes directions" $ do
    getDirection (mkDirection 3) `shouldBe` 3
    getType (mkType 1) `shouldBe` 1
    getNumber (mkNumber 1) `shouldBe` 1
    getSize (mkSize 1) `shouldBe` 1

    getDirection #{const _IOWR(1, 1, char)} `shouldBe` 3
    getType #{const _IOWR(1, 1, char)} `shouldBe` 1
    getNumber #{const _IOWR(1, 1, char)} `shouldBe` 1
    getSize #{const _IOWR(1, 1, char)} `shouldBe` 1

  it "masks" $ do
    IOC_WRITE `shouldBe` 1
    IOC_READ `shouldBe` 2
    IOC_WRITEREAD `shouldBe` 3
