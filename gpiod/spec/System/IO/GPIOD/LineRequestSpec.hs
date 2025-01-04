module System.IO.GPIOD.LineRequestSpec where

import Control.Monad.IO.Class
import Test.Hspec
import Test.Hspec.Hedgehog

import System.IO.GPIOD.Line (lookupLineByName)
import System.IO.GPIOD.LineRequest
import System.IO.GPIOD.LineRequest.EdgeDetection
import System.IO.GPIOD.ReservedLine
import System.IO.GPIOD.TestUtils
import qualified System.IO.GPIOD.Gen as Gen

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  around withSim $ do

    it "reserves a line for input" $ \_di -> do
      line <- ensureRightJust $ lookupLineByName "daphne"
      let req = inputRequest "test-input-consumer"
      request line req >>= \case
        Left err -> error $ show err
        Right reservedLine -> release reservedLine
      () `shouldBe` ()

    it "reserves a line for output" $ \_di -> do
      line <- ensureRightJust $ lookupLineByName "daphne"
      let req = outputRequest "test-output-consumer" Low
      request line req >>= \case
        Left err -> error $ show err
        Right reservedLine -> release reservedLine
      () `shouldBe` ()

    it "reserves a line for events" $ \_di -> do
      line <- ensureRightJust $ lookupLineByName "daphne"
      let req = eventsRequest "test-events-consumer"
      request line (req :: LineRequest ('Events 'BothEdges)) >>= \case
        Left err -> error $ show err
        Right reservedLine -> release reservedLine
      () `shouldBe` ()

    it "reserves a line twice" $ \_di -> do
      line <- ensureRightJust $ lookupLineByName "daphne"
      let req = outputRequest "test-output-consumer" Low
      request line req >>= \case
        Left err -> error $ show err
        Right reservedLine -> release reservedLine
      request line req >>= \case
        Left err -> error $ show err
        Right reservedLine -> release reservedLine
      () `shouldBe` ()

    it "reserves a line with any input LineRequest" \_di -> hedgehog do
      req <- forAll Gen.inputRequest
      liftIO $ do
        line <- ensureRightJust $ lookupLineByName "daphne"
        rl <- ensureRight $ request line req
        release rl

    it "reserves a line with any output LineRequest" \_di -> hedgehog do
      req <- forAll Gen.outputRequest
      liftIO $ do
        line <- ensureRightJust $ lookupLineByName "daphne"
        rl <- ensureRight $ request line req
        release rl

    it "reserves a line with any event LineRequest" \_di -> hedgehog do
      req :: LineRequest ('Events 'BothEdges) <- forAll Gen.eventsRequest
      liftIO $ do
        line <- ensureRightJust $ lookupLineByName "daphne"
        rl <- ensureRight $ request line req
        release rl

    it "reserves a line for input and then reads a value from it" \di -> do
      line <- ensureRightJust $ lookupLineByName "daphne"
      let req = (inputRequest "test-input-consumer")
            { bias = Just PullDown
            }
      ensureRight $ withReservedLine line req $ \rl -> do
        getValue rl `shouldReturn` Right Low
        setSimLinePullByName di "daphne" High
        getValue rl `shouldReturn` Right High

    it "reserves a line for output and then reads a value from it" \_di -> do
      line <- ensureRightJust $ lookupLineByName "daphne"
      let req = outputRequest "test-output-consumer" Low
      ensureRight $ withReservedLine line req $ \rl -> do
        getValue rl `shouldReturn` Right Low

    it "reserves a line for events and then reads a value from it" \_di -> do
      line <- ensureRightJust $ lookupLineByName "daphne"
      let req :: LineRequest ('Events 'BothEdges)
          req = eventsRequest "test-events-consumer"
      ensureRight $ withReservedLine line req $ \rl -> do
        getValue rl `shouldReturn` Right Low

    it "reserves a line for output and then writes a value to it" \_di -> do
      line <- ensureRightJust $ lookupLineByName "daphne"
      let req = outputRequest "test-output-consumer" Low
      ensureRight $ withReservedLine line req $ \rl -> do
        getValue rl `shouldReturn` Right Low
        setValue rl High `shouldReturn` Right ()
        getValue rl `shouldReturn` Right High

    -- it "reserves a line for input and then writes a value to it" \di -> do
    --   line <- ensureRightJust $ lookupLineByName "daphne"
    --   let req = inputRequest "test-input-consumer"
    --   ensureRight $ withReservedLine line req $ \rl -> do
    --     setValue rl High `shouldReturn`
    --       Left (gpiodError LineOperationNotPermitted)

    it "reserves a line for events and then registers a listener to it" \_di -> do
      line <- ensureRightJust $ lookupLineByName "daphne"
      let req :: LineRequest ('Events 'BothEdges)
          req = eventsRequest "test-events-consumer"
      ensureRight $ withReservedLine line req $ \rl -> do
        _eh <- watchLineEvents print rl
        pure ()

      () `shouldBe` ()