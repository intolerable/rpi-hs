module Main where

import Test.Hspec

import qualified System.IO.SPIDevSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "SPIDev" System.IO.SPIDevSpec.spec
