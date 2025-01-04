module System.IO.GPIOD.LineRequest.Drive where

import Data.Bits

import qualified LibGPIOD

data Drive
  = PushPull
  | OpenDrain
  | OpenSource
  deriving (Show, Eq, Ord, Enum, Bounded)

mkDriveFlag :: Drive -> LibGPIOD.GPIODLineRequestFlag
mkDriveFlag = \case
  PushPull -> zeroBits
  OpenSource -> LibGPIOD.GPIODLineRequestFlagOpenSource
  OpenDrain -> LibGPIOD.GPIODLineRequestFlagOpenDrain
