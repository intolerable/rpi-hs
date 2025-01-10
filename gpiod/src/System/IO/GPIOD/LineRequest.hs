module System.IO.GPIOD.LineRequest
  ( ReservedLine
  , withReservedLine
  , request
  , release
  , LineRequest(..)
  , RequestType(..)
  , KnownRequestType(..)
  , EdgeDetectionType(..)
  , KnownEdgeDetectionType(..)
  , inputRequest
  , outputRequest
  , eventsRequest
  , setBias
  , setActiveState
  , ActiveState(..)
  , Bias(..)
  , OutputOnly(..)
  , Drive(..)
  ) where

import Control.Exception
import Data.Bits
import Data.Foldable
import Data.Proxy
import Data.Text (Text)
import Foreign.C.ConstPtr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import qualified Data.Text.Foreign as Text.Foreign

import System.IO.GPIOD.Error
import System.IO.GPIOD.Line (Line, ActiveState(..), withLinePtr)
import System.IO.GPIOD.LineRequest.Bias (Bias(..), biasLineRequestFlag)
import System.IO.GPIOD.LineRequest.Drive
import System.IO.GPIOD.LineRequest.EdgeDetection
import System.IO.GPIOD.LineRequest.RequestType
import System.IO.GPIOD.ReservedLine (ReservedLine(..))
import qualified System.IO.LibGPIOD as LibGPIOD

withReservedLine :: KnownRequestType rt
                 => Line
                 -> LineRequest rt
                 -> (ReservedLine rt -> IO a)
                 -> IO (Either (GPIODError '[LineAlreadyReserved]) a)
withReservedLine l lrc f =
  bracket
    (request l lrc)
    (either (const (pure ())) release)
    (traverse f)

release :: ReservedLine rt -> IO ()
release (ReservedLine fptr _) =
  withForeignPtr fptr LibGPIOD.gpiod_line_release

request :: KnownRequestType rt
        => Line
        -> LineRequest rt
        -> IO (Either (GPIODError '[LineAlreadyReserved]) (ReservedLine rt))
request l lrc = do
  withLineRequest lrc $ \lrcptr ->
    withLinePtr l $ \lptr -> do
      let initialValue =
            case defaultValue lrc of
              OutputOnlyInput -> 0
              OutputOnlyEvents -> 0
              OutputOnlyOutput High -> 1
              OutputOnlyOutput Low -> 0
      bracketOnError
        (LibGPIOD.gpiod_line_request lptr lrcptr initialValue)
        (\_ -> LibGPIOD.gpiod_line_release lptr) $ \res ->
          fromCInt "requestLine" res $ do
            fptr <- newForeignPtr LibGPIOD.gpiod_line_release_p lptr
            pure $ ReservedLine fptr l

data LineRequest (rt :: RequestType) = LineRequest
  { consumerName :: Text
  , activeState :: ActiveState
  , bias :: Maybe Bias
  , drive :: OutputOnly rt Drive
  , defaultValue :: OutputOnly rt ActiveState
  } deriving (Show, Eq, Ord)

inputRequest :: Text -> LineRequest 'Input
inputRequest cname = LineRequest
  { consumerName = cname
  , activeState = High
  , bias = Nothing
  , drive = OutputOnlyInput
  , defaultValue = OutputOnlyInput
  }

outputRequest :: Text -> ActiveState -> LineRequest 'Output
outputRequest cname d = LineRequest
  { consumerName = cname
  , activeState = High
  , bias = Nothing
  , drive = OutputOnlyOutput PushPull
  , defaultValue = OutputOnlyOutput d
  }

eventsRequest :: Text -> LineRequest ('Events edt)
eventsRequest cname = LineRequest
  { consumerName = cname
  , activeState = High
  , bias = Nothing
  , drive = OutputOnlyEvents
  , defaultValue = OutputOnlyEvents
  }

setActiveState :: ActiveState -> LineRequest rt -> LineRequest rt
setActiveState as lrc = lrc
  { activeState = as
  }

setBias :: Maybe Bias -> LineRequest rt -> LineRequest rt
setBias b lrc = lrc
  { bias = b
  }

withLineRequest :: KnownRequestType rt
                => LineRequest rt
                -> (ConstPtr LibGPIOD.GPIODLineRequestConfig -> IO a)
                -> IO a
withLineRequest (lrc :: LineRequest rt) f =
  Text.Foreign.withCString (consumerName lrc) $ \str ->
    alloca $ \lrcptr -> do
      poke lrcptr $ LibGPIOD.GPIODLineRequestConfig
        { consumer = ConstPtr str
        , requestType = requestTypeVal' (Proxy :: Proxy rt)
        , flags = mkLineRequestFlag lrc
        }
      f (ConstPtr lrcptr)

mkLineRequestFlag :: LineRequest rt -> LibGPIOD.GPIODLineRequestFlag
mkLineRequestFlag lrc = foldl' (.|.) zeroBits
  [ biasLineRequestFlag (bias lrc)
  , case drive lrc of
      OutputOnlyInput -> zeroBits
      OutputOnlyEvents -> zeroBits
      OutputOnlyOutput PushPull -> zeroBits
      OutputOnlyOutput OpenSource -> LibGPIOD.GPIODLineRequestFlagOpenSource
      OutputOnlyOutput OpenDrain -> LibGPIOD.GPIODLineRequestFlagOpenDrain
  , case activeState lrc of
      Low -> LibGPIOD.GPIODLineRequestFlagActiveLow
      High -> zeroBits
  ]
