module System.IO.GPIOD.Gen
  ( inputRequest
  , outputRequest
  , eventsRequest
  ) where

import Data.Text (Text)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified System.IO.GPIOD.LineRequest as LR
import System.IO.GPIOD.LineRequest (LineRequest, RequestType(..), Bias, ActiveState, Drive)

inputRequest :: MonadGen m => m (LineRequest 'Input)
inputRequest =
  LR.LineRequest <$> consumerName
                 <*> activeState
                 <*> Gen.maybe bias
                 <*> pure LR.OutputOnlyInput
                 <*> pure LR.OutputOnlyInput

outputRequest :: MonadGen m => m (LineRequest 'Output)
outputRequest =
  LR.LineRequest <$> consumerName
                 <*> activeState
                 <*> Gen.maybe bias
                 <*> (LR.OutputOnlyOutput <$> drive)
                 <*> (LR.OutputOnlyOutput <$> activeState)

eventsRequest :: MonadGen m => m (LineRequest (Events ed))
eventsRequest =
  LR.LineRequest <$> consumerName
                 <*> activeState
                 <*> Gen.maybe bias
                 <*> pure LR.OutputOnlyEvents
                 <*> pure LR.OutputOnlyEvents

consumerName :: MonadGen m => m Text
consumerName = Gen.text (Range.exponential 0 128) Gen.ascii

activeState :: MonadGen m => m ActiveState
activeState = Gen.enumBounded

bias :: MonadGen m => m Bias
bias = Gen.enumBounded

drive :: MonadGen m => m Drive
drive = Gen.enumBounded
