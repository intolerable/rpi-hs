module Main where

import Test.Hspec

import qualified IOCtl.RawSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "IOCtl.RawSpec" IOCtl.RawSpec.spec
