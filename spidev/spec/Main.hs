module Main where

import Test.Hspec

import qualified SPIDevSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "SPIDev" SPIDevSpec.spec
