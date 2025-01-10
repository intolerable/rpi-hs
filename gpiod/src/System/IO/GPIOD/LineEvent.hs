module System.IO.GPIOD.LineEvent where

import System.Clock (TimeSpec)

import qualified System.IO.LibGPIOD as LibGPIOD

data EventType
  = RisingEdge
  | FallingEdge
  deriving (Show, Eq, Ord)

data LineEvent = LineEvent
  { timestamp :: TimeSpec
  , eventType :: EventType
  } deriving (Show, Eq, Ord)

fromGPIODLineEvent :: LibGPIOD.GPIODLineEvent -> LineEvent
fromGPIODLineEvent gle = LineEvent
  { timestamp = LibGPIOD.timespec gle
  , eventType = case LibGPIOD.eventType gle of
      LibGPIOD.GPIODLineEventRisingEdge -> RisingEdge
      LibGPIOD.GPIODLineEventFallingEdge -> FallingEdge
  }
