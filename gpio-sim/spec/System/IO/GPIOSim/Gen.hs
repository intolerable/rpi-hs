module System.IO.GPIOSim.Gen where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import System.IO.GPIOSim (FileAction(..), Direction)

fileAction :: MonadGen m => m a -> m (FileAction a)
fileAction a = Gen.choice
  [ CreateDirectory <$> a
  , WriteText <$> a <*> Gen.text (Range.linear 4 16) Gen.ascii
  , WriteDirection <$> a <*> direction
  , WriteBool <$> a <*> Gen.bool
  , RemoveDirectory <$> a
  ]

direction :: MonadGen m => m Direction
direction = Gen.enumBounded
