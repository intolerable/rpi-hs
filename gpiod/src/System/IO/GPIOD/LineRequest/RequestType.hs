module System.IO.GPIOD.LineRequest.RequestType where
-- TODO: move this to System.IO.GPIOD.LineType

import Data.Proxy

import System.IO.GPIOD.LineRequest.EdgeDetection
import qualified System.IO.LibGPIOD as LibGPIOD

data RequestType
  = Input
  | Events EdgeDetectionType
  | Output
  deriving (Show, Eq, Ord)

class KnownRequestType (rt :: RequestType) where
  requestTypeVal :: Proxy rt -> RequestType

instance KnownRequestType 'Input where
  requestTypeVal _ = Input

instance KnownEdgeDetectionType edt => KnownRequestType ('Events edt) where
  requestTypeVal (Proxy :: Proxy ('Events edt)) =
    Events (edgeDetectionTypeVal (Proxy :: Proxy edt))

instance KnownRequestType 'Output where
  requestTypeVal _ = Output

requestTypeVal' :: KnownRequestType rt
                => Proxy rt
                -> LibGPIOD.GPIODLineRequestType
requestTypeVal' p =
  case requestTypeVal p of
    Input -> LibGPIOD.GPIODLineRequestDirectionInput
    Output -> LibGPIOD.GPIODLineRequestDirectionOutput
    Events FallingEdge -> LibGPIOD.GPIODLineRequestEventFallingEdge
    Events RisingEdge -> LibGPIOD.GPIODLineRequestEventRisingEdge
    Events BothEdges -> LibGPIOD.GPIODLineRequestEventBothEdges

data OutputOnly (rt :: RequestType) a where
  OutputOnlyInput :: OutputOnly 'Input a
  OutputOnlyEvents :: OutputOnly ('Events edt) a
  OutputOnlyOutput :: a -> OutputOnly 'Output a

deriving instance Show a => Show (OutputOnly rt a)
deriving instance Eq a => Eq (OutputOnly rt a)
deriving instance Ord a => Ord (OutputOnly rt a)
