module System.IO.GPIOSimSpec where

import Test.Hspec
import Test.Hspec.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import System.IO.GPIOSim
import qualified System.IO.GPIOSim.Gen as Gen

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "intercalate" $ do

    specify "intercalate mempty mempty == mempty" $ do
      intercalate mempty (mempty :: [String]) `shouldBe` mempty

    specify "intercalate x mempty == mempty" $ hedgehog $ do
      x <- forAll $ Gen.string (Range.exponential 0 16) Gen.unicode
      intercalate x (mempty :: [String]) === mempty

    specify "length (intercalate x ys) >= length (mconcat ys)" $ hedgehog $ do
      x <- forAll $ Gen.string (Range.exponential 0 16) Gen.unicode
      ys <- forAll $ Gen.list (Range.exponential 0 16) $
        Gen.string (Range.exponential 0 16) Gen.unicode
      assert $ length (intercalate x ys) >= length (mconcat ys)

    specify "length (intercalate x ys) == length (mconcat ys) + (length x * length ys)" $ hedgehog $ do
      x <- forAll $ Gen.string (Range.exponential 0 16) Gen.unicode
      ys <- forAll $ Gen.list (Range.exponential 0 16) $
        Gen.string (Range.exponential 0 16) Gen.unicode
      assert $ length (intercalate x ys) == length (mconcat ys) + (length x * length ys)

  describe "FileAction" $ do

    describe "invert" $ do

      specify "(invert . invert) ==~ id" $ hedgehog $ do
        fa <- forAll $ Gen.fileAction $
          Gen.string (Range.linear 4 16) Gen.ascii
        case invert fa >>= invert of
          Nothing -> pure ()
          Just inverted -> inverted === fa
