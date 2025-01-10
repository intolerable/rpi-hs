module Main where

import Test.Hspec

import qualified System.IO.IOCtl.RawSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "IOCtl.RawSpec" System.IO.IOCtl.RawSpec.spec
