module System.IO.GPIOD.ChipSpec where

import Data.Text (Text)
import Control.Monad
import Test.Hspec
import qualified Data.Text as Text
import qualified Streaming.Prelude as S

import System.IO.GPIOD.Chip
import System.IO.GPIOD.Line
import System.IO.GPIOD.TestUtils
import qualified System.IO.GPIOSim as Sim

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  aroundAll withSim $ do

    it "finds simulated chips" \di -> do
      forM_ (zip (Sim.bankInfos di) [0..]) \(bi, bankIndex) -> do
        lookupChipByName (Sim.chipName bi) >>= \case
          Left err -> expectationFailure $ show (bi, err)
          Right Nothing -> expectationFailure $ "Missing chip: " <> show (Sim.chipName bi)
          Right (Just c) -> do
            res <- getChipInfo c
            res `shouldBe` ChipInfo
              { name = Sim.chipName bi
              , label =
                  expectedChipLabel di bankIndex
              , lineCount = 16
              }

    it "finds simulated chips' lines" \di -> do
      forM_ (Sim.bankInfos di) \bi -> do
        lookupChipByName (Sim.chipName bi) >>= \case
          Left err ->
            expectationFailure $ show (bi, err)
          Right Nothing ->
            expectationFailure $ "Missing chip: " <> show (Sim.chipName bi)
          Right (Just c) -> do
            ls <- S.toList_ $ S.mapM getLineInfo $ allChipLines c
            length ls `shouldBe` 16

expectedChipLabel :: Sim.DeviceInfo -> Word -> Text
expectedChipLabel di ix =
  Sim.devName di <> ":node" <> Text.pack (show ix)
