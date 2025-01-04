module System.IO.GPIOD.ReservedLine
  ( ReservedLine(..)
  , getValue
  , setValue
  , EventHandler
  , unregisterEventHandler
  , eventHandlerHasPendingEvents
  , waitEventHandler
  , watchLineEvents
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Data.Bits
import Data.Foldable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
-- import System.Posix.Types (Fd)
import qualified GHC.IO.Device as Device
import GHC.IO.FD
import GHC.IO.IOMode

import System.IO.GPIOD.Line (Line, ActiveState(..))
import System.IO.GPIOD.LineEvent
import System.IO.GPIOD.Error
import System.IO.GPIOD.LineRequest.RequestType
import qualified LibGPIOD

data ReservedLine (rt :: RequestType) =
  ReservedLine (ForeignPtr LibGPIOD.GPIODLine) Line

withReservedLinePtr :: ReservedLine rt -> (Ptr LibGPIOD.GPIODLine -> IO a) -> IO a
withReservedLinePtr (ReservedLine fptr _l) f = withForeignPtr fptr f

getValue :: ReservedLine rt -> IO (Either (GPIODError '[]) ActiveState)
getValue rl = withReservedLinePtr rl $ \ptr ->
  fromCIntIO "getValue" (LibGPIOD.gpiod_line_get_value ptr) $ \cint ->
    pure $ if cint `testBit` 0 then High else Low

setValue :: ReservedLine 'Output -> ActiveState -> IO (Either (GPIODError '[LineOperationNotPermitted]) ())
setValue rl as = withReservedLinePtr rl $ \ptr -> do
  let v = case as of
        High -> 1
        Low -> 0
  fromCIntIO "setValue" (LibGPIOD.gpiod_line_set_value ptr v) $ \_cint ->
    pure ()

data EventHandler =
  EventHandler (ForeignPtr LibGPIOD.GPIODLine) (Async ()) (TVar Bool)

unregisterEventHandler :: EventHandler -> IO ()
unregisterEventHandler (EventHandler _ a _) = cancel a

waitEventHandler :: EventHandler -> IO ()
waitEventHandler (EventHandler _ a _) = wait a

eventHandlerHasPendingEvents :: EventHandler -> STM Bool
eventHandlerHasPendingEvents (EventHandler _ _ tvar) = readTVar tvar

watchLineEvents :: (Either (GPIODError '[]) LineEvent -> IO ())
                -> ReservedLine ('Events edt)
                -> IO EventHandler
watchLineEvents f (ReservedLine rl _) = do
  withForeignPtr rl $ \lptr -> do
    cintFd <- LibGPIOD.gpiod_line_event_get_fd lptr
    let fd = FD cintFd 0
    tvar <- newTVarIO True
    a <- async $ do
      threadWaitLoop tvar fd do
        r <- alloca $ \leptr -> do
          fromCIntIO "watchLineEvents" (LibGPIOD.gpiod_line_event_read_fd (fdFD fd) leptr) $ \_c -> do
              peek leptr
        f (fmap fromGPIODLineEvent r)
    link a
    pure $ EventHandler rl a tvar

threadWaitLoop :: TVar Bool -> FD -> IO () -> IO ()
threadWaitLoop tvar fd f = forever $ do
  isReady <- Device.ready fd False 0
  unless isReady $ do
    atomically $ writeTVar tvar False
    threadWaitRead $ fromIntegral $ fdFD fd
    atomically $ writeTVar tvar True
  f

