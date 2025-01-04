module System.IO.GPIOD.LineRequest.EdgeDetection where

import Data.Proxy

data EdgeDetectionType
  = FallingEdge
  | RisingEdge
  | BothEdges
  deriving (Show, Eq, Ord)

class KnownEdgeDetectionType (edt :: EdgeDetectionType) where
  edgeDetectionTypeVal :: Proxy edt -> EdgeDetectionType

instance KnownEdgeDetectionType 'FallingEdge where edgeDetectionTypeVal _ = FallingEdge
instance KnownEdgeDetectionType 'RisingEdge where edgeDetectionTypeVal _ = RisingEdge
instance KnownEdgeDetectionType 'BothEdges where edgeDetectionTypeVal _ = BothEdges
