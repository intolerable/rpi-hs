module System.IO.GPIOD.LineRequest.Bias where

import Data.Bits
import qualified LibGPIOD

data Bias
  = Disable
  | PullUp
  | PullDown
  deriving (Show, Eq, Ord, Enum, Bounded)

biasLineRequestFlag :: Maybe Bias -> LibGPIOD.GPIODLineRequestFlag
biasLineRequestFlag = \case
  Nothing -> zeroBits
  Just Disable -> LibGPIOD.GPIODLineRequestFlagBiasDisable
  Just PullUp -> LibGPIOD.GPIODLineRequestFlagBiasPullUp
  Just PullDown -> LibGPIOD.GPIODLineRequestFlagBiasPullDown
