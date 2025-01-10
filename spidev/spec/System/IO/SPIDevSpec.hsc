module System.IO.SPIDevSpec where

import Control.Monad
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import System.IO.SPIDev

#include "linux/spi/spidev.h"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "spiMessageSize" $ do

    it "matches SPI_MSGSIZE" $ do
      let sizes :: [(Integer, Integer)]
          sizes =
            [ (1, #{const SPI_MSGSIZE(1)})
            , (2, #{const SPI_MSGSIZE(2)})
            , (3, #{const SPI_MSGSIZE(3)})
            , (4, #{const SPI_MSGSIZE(4)})
            , (5, #{const SPI_MSGSIZE(5)})
            , (6, #{const SPI_MSGSIZE(6)})
            , (7, #{const SPI_MSGSIZE(7)})
            , (8, #{const SPI_MSGSIZE(8)})
            , (16, #{const SPI_MSGSIZE(16)})
            , (32, #{const SPI_MSGSIZE(32)})
            , (64, #{const SPI_MSGSIZE(64)})
            ]
      forM_ sizes $ \(i, s) -> do
        spiMessageSize i `shouldBe` Just (fromIntegral s)

      spiMessageSize 8192 `shouldBe` Nothing

  describe "Mode" $ do

    specify "modeWord8 (word8Mode w) == w" $ hedgehog $ do
      w <- forAll $ Gen.integral Range.linearBounded
      modeWord8 (word8Mode w) === w
