module System.IO.GPIOD.TestUtils
  ( withSim
  , ensureRight
  , ensureRightJust
  , setSimLinePullByName
  ) where

import Data.Text (Text)
import Control.Monad
import Data.Monoid (First(..))
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as Text.IO
import qualified Data.Text as Text

import System.IO.GPIOD.Line (ActiveState(..))
import System.IO.GPIOD.Error
import System.IO.GPIOSim

withSim :: (DeviceInfo -> IO a) -> IO a
withSim =
  withDeviceInfo defaultGPIOSimOptions exampleDeviceSpec

ensureRight :: Show (GPIODError xs) => IO (Either (GPIODError xs) a) -> IO a
ensureRight = ensureRightJust . fmap (fmap Just)

ensureRightJust :: Show (GPIODError xs) => IO (Either (GPIODError xs) (Maybe a)) -> IO a
ensureRightJust act = act >>= \case
  Left err -> do
    expectationFailure $ "Unexpected error: " <> show err
    error "ensureRightJust"
  Right Nothing -> do
    expectationFailure $ "Unexpected Nothing"
    error "ensureRightJust"
  Right (Just x) -> pure x

setSimLinePullByName :: DeviceInfo -> LineName -> ActiveState -> IO ()
setSimLinePullByName di ln as =
  case simFindLineByName di ln of
    Nothing -> error $ "setSimLineValueByName: Could not locate line with name " <> show ln
    Just (dn, cn, LineOffset o) -> do
      Text.IO.writeFile ("/sys/devices/platform/" <> Text.unpack dn <> "/" <> Text.unpack cn <> "/sim_gpio" <> show o <> "/pull") $
        case as of
          High -> "pull-up"
          Low -> "pull-down"

simFindLineByName :: DeviceInfo -> LineName -> Maybe (Text, Text, LineOffset)
simFindLineByName di ln = do
  let allBanks = fmap (\bi -> (chipName bi, bankSpec bi)) (bankInfos di)
      findFirstMapValue = Map.foldMapWithKey $ \k v -> First $ do
        guard (v == ln)
        pure k
  getFirst $ foldMap (\(n, b) -> (,,) (devName di) n <$> findFirstMapValue (lineNames b)) allBanks
