module System.IO.GPIOD.LineSpec where

import Test.Hspec

import System.IO.GPIOD.Line
import System.IO.GPIOD.TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  aroundAll withSim $ do

    it "finds lines by name" $ \_di -> do
      lookupLineByName "albert" >>= \case
        Left err -> expectationFailure $ show err
        Right Nothing -> expectationFailure $ "Missing line: " <> "albert"
        Right (Just l) -> do
          getLineInfo l `shouldReturn` LineInfo
            { offset = 0
            , name = Just "albert"
            , consumer = Just "albert-hog"
            , direction = Output
            , activeState = High
            , bias = AsIs
            , used = True
            , openDrain = False
            , openSource = False
            }
      lookupLineByName "brenda" >>= \case
        Left err -> expectationFailure $ show err
        Right Nothing -> expectationFailure $ "Missing line: " <> "brenda"
        Right (Just l) -> do
          getLineInfo l `shouldReturn` LineInfo
            { offset = 1
            , name = Just "brenda"
            , consumer = Just "brenda-hog"
            , direction = Output
            , activeState = High
            , bias = AsIs
            , used = True
            , openDrain = False
            , openSource = False
            }
      lookupLineByName "christine" >>= \case
        Left err -> expectationFailure $ show err
        Right Nothing -> expectationFailure $ "Missing line: " <> "christine"
        Right (Just l) -> do
          getLineInfo l `shouldReturn` LineInfo
            { offset = 2
            , name = Just "christine"
            , consumer = Just "christine-hog"
            , direction = Input
            , activeState = High
            , bias = AsIs
            , used = True
            , openDrain = False
            , openSource = False
            }
      lookupLineByName "daphne" >>= \case
        Left err -> expectationFailure $ show err
        Right Nothing -> expectationFailure $ "Missing line: " <> "daphne"
        Right (Just l) -> do
          getLineInfo l `shouldReturn` LineInfo
            { offset = 3
            , name = Just "daphne"
            , consumer = Nothing
            , direction = Input
            , activeState = High
            , bias = AsIs
            , used = False
            , openDrain = False
            , openSource = False
            }
