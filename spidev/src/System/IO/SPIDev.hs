module System.IO.SPIDev
  ( Bus
  , withBus
  , open
  , close
  , mkSPI
  , SPIError(..)
  , readBytes
  , writeBytes
  , readBytesPtr
  , writeBytesPtr
  , Mode(..)
  , getMode
  , setMode
  , modifyMode
  , modifyMode'
  , modeWord8
  , word8Mode
  , MaxSpeedHz(..)
  , getMaxSpeedHz
  , setMaxSpeedHz
  , modifyMaxSpeedHz
  , modifyMaxSpeedHz'
  , BitsPerWord(..)
  , getBitsPerWord
  , setBitsPerWord
  , modifyBitsPerWord
  , modifyBitsPerWord'
  , SPIIOCMessage(..)
  , mkSPIIOCMessage
  , spiMessageSize'
  , spiMessageSize
  ) where

import Data.ByteString (ByteString)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Proxy
import Data.Word
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import GHC.IO.FD (FD(..))
import GHC.IO.IOMode
import GHC.TypeLits
import Foreign.Ptr
import qualified GHC.IO.FD as FD
import qualified Data.ByteString as ByteString

import System.IO.SPIDev.Raw (SPIIOCTransfer(..))
import qualified System.IO.SPIDev.Raw as Raw
import System.IO.IOCtl.Raw

data Mode = Mode
  { cpha :: Bool
  , cpol :: Bool
  , csHigh :: Bool
  , lsbFirst :: Bool
  , threeWire :: Bool
  , loop :: Bool
  , noCS :: Bool
  , ready :: Bool
  } deriving (Show, Eq, Ord)

modeWord8 :: Mode -> Word8
modeWord8 m = foldl (.|.) zeroBits
  [ if cpha m then bit 0 else zeroBits
  , if cpol m then bit 1 else zeroBits
  , if csHigh m then bit 2 else zeroBits
  , if lsbFirst m then bit 3 else zeroBits
  , if threeWire m then bit 4 else zeroBits
  , if loop m then bit 5 else zeroBits
  , if noCS m then bit 6 else zeroBits
  , if ready m then bit 7 else zeroBits
  ]

word8Mode :: Word8 -> Mode
word8Mode w =
  Mode (w `testBit` 0)
       (w `testBit` 1)
       (w `testBit` 2)
       (w `testBit` 3)
       (w `testBit` 4)
       (w `testBit` 5)
       (w `testBit` 6)
       (w `testBit` 7)

data Bus = Bus (MVar SPIState)

data SPIState = SPIState
  { stateFd :: FD
  , stateMode :: Mode
  , stateMaxSpeedHz :: MaxSpeedHz
  , stateBitsPerWord :: BitsPerWord
  } deriving (Show)

withBus :: FilePath -> (Bus -> IO a) -> IO (Either SPIError a)
withBus fp act =
  bracket
    (open fp)
    (either (\_ -> pure ()) close)
    (either (pure . Left) (fmap Right . act))

open :: FilePath -> IO (Either SPIError Bus)
open fp = do
  fd <- FD.openFileWith fp ReadWriteMode True (\fd _ -> pure fd) (\_ fd -> pure fd)
  mkSPI fd

close :: Bus -> IO ()
close (Bus mvar) = do
  s <- takeMVar mvar
  FD.release $ stateFd s

mkSPI :: FD -> IO (Either SPIError Bus)
mkSPI fd = runSPIExcept $ do
  m <- SPIExcept $ fdGetMode fd
  s <- SPIExcept $ fdGetMaxSpeedHz fd
  b <- SPIExcept $ fdGetBitsPerWord fd
  mvar <- fromIO $ newMVar $ SPIState
    { stateFd = fd
    , stateMode = m
    , stateMaxSpeedHz = s
    , stateBitsPerWord = b
    }
  pure $ Bus mvar

getMode :: Bus -> IO (Either SPIError Mode)
getMode spi = modifyMode' (\m -> (m, m)) spi

setMode :: Mode -> Bus -> IO (Either SPIError ())
setMode m spi = modifyMode (const m) spi

modifyMode :: (Mode -> Mode) -> Bus -> IO (Either SPIError ())
modifyMode f spi = modifyMode' (pure . f) spi

modifyMode' :: (Mode -> (a, Mode)) -> Bus -> IO (Either SPIError a)
modifyMode' f (Bus mvar) = modifyMVar mvar $ \st -> do
  let oldMode = stateMode st
      (v, newMode) = f oldMode

  res <- if (newMode == oldMode)
    then pure $ Right v
    else
      fmap (v <$) $ fdSetMode (stateFd st) newMode
  pure (st { stateMode = newMode }, res)

newtype MaxSpeedHz = MaxSpeedHz Word32
  deriving (Show, Eq, Ord)

getMaxSpeedHz :: Bus -> IO (Either SPIError MaxSpeedHz)
getMaxSpeedHz = modifyMaxSpeedHz' (\m -> (m, m))

setMaxSpeedHz :: MaxSpeedHz -> Bus -> IO (Either SPIError ())
setMaxSpeedHz m = modifyMaxSpeedHz (const m)

modifyMaxSpeedHz :: (MaxSpeedHz -> MaxSpeedHz) -> Bus -> IO (Either SPIError ())
modifyMaxSpeedHz f spi = modifyMaxSpeedHz' (pure . f) spi

modifyMaxSpeedHz' :: (MaxSpeedHz -> (a, MaxSpeedHz)) -> Bus -> IO (Either SPIError a)
modifyMaxSpeedHz' f (Bus mvar) = modifyMVar mvar $ \st -> do
  let oldMaxSpeedHz = stateMaxSpeedHz st
      (v, newMaxSpeedHz) = f oldMaxSpeedHz

  res <- if newMaxSpeedHz == oldMaxSpeedHz
    then pure $ Right v
    else
      fmap (v <$) $ fdSetMaxSpeedHz (stateFd st) newMaxSpeedHz
  pure (st { stateMaxSpeedHz = newMaxSpeedHz }, res)

newtype BitsPerWord = BitsPerWord Word32
  deriving (Show, Eq, Ord)

getBitsPerWord :: Bus -> IO (Either SPIError BitsPerWord)
getBitsPerWord = modifyBitsPerWord' (\b -> (b, b))

setBitsPerWord :: BitsPerWord -> Bus -> IO (Either SPIError ())
setBitsPerWord b = modifyBitsPerWord (const b)

modifyBitsPerWord :: (BitsPerWord -> BitsPerWord) -> Bus -> IO (Either SPIError ())
modifyBitsPerWord f spi = modifyBitsPerWord' (pure . f) spi

modifyBitsPerWord' :: (BitsPerWord -> (a, BitsPerWord)) -> Bus -> IO (Either SPIError a)
modifyBitsPerWord' f (Bus mvar) = modifyMVar mvar $ \st -> do
  let oldBitsPerWord = stateBitsPerWord st
      (v, newBitsPerWord) = f oldBitsPerWord

  res <- if newBitsPerWord == oldBitsPerWord
    then pure $ Right v
    else
      fmap (v <$) $ fdSetBitsPerWord (stateFd st) newBitsPerWord
  pure (st { stateBitsPerWord = newBitsPerWord }, res)

readBytes :: Int -> Bus -> IO (Either SPIError ByteString)
readBytes n bus = do
  allocaBytes n $ \str -> do
    ret <- readBytesPtr str n bus
    case ret of
      Left err -> pure $ Left err
      Right c ->
        fmap Right $ ByteString.packCStringLen (str, c)

writeBytes :: ByteString -> Bus -> IO (Either SPIError ())
writeBytes bs bus =
  ByteString.useAsCStringLen bs $ \(str, l) -> do
    writeBytesPtr str l bus

readBytesPtr :: Ptr a -> Int -> Bus -> IO (Either SPIError Int)
readBytesPtr ptr ptrLen (Bus mvar) = withMVar mvar $ \st -> do
  calloca $ \tptr -> do
    poke tptr Raw.SPIIOCTransfer
      { txBuf = castPtrWord64 nullPtr
      , rxBuf = castPtrWord64 ptr

      -- TODO: error if negative or exceeds max bounds for Word32
      , len = fromIntegral ptrLen
      , speedHz = 0

      , delayUsecs = 0
      , bitsPerWord = 0
      , txNBits = 0
      , rxNBits = 0
      , wordDelayUsecs = 0
      , csChange = 0
      , pad = 0
      }
    let SPIIOCMessage cmd =
          mkSPIIOCMessage (Proxy :: Proxy 1)
    fmap (fmap fromIntegral) $
      runSPIExcept $ exceptErrnoIfMinus1Retry "readBytesPtr" $
        Raw.ioctl (fdFD $ stateFd st) cmd tptr

writeBytesPtr :: Ptr a -> Int -> Bus -> IO (Either SPIError ())
writeBytesPtr ptr ptrLen (Bus mvar) = withMVar mvar $ \st -> do
  calloca $ \tptr -> do
    poke tptr Raw.SPIIOCTransfer
      { txBuf = castPtrWord64 ptr
      , rxBuf = castPtrWord64 nullPtr

      -- TODO: error if negative or exceeds max bounds for Word32
      , len = fromIntegral ptrLen
      , speedHz = 0

      , delayUsecs = 0
      , bitsPerWord = 0
      , txNBits = 0
      , rxNBits = 0
      , wordDelayUsecs = 0
      , csChange = 0
      , pad = 0
      }
    let SPIIOCMessage cmd =
          mkSPIIOCMessage (Proxy :: Proxy 1)
    runSPIExcept $ exceptErrnoIfMinus1Retry_ "writeBytesPtr" $
      Raw.ioctl (fdFD $ stateFd st) cmd tptr

newtype SPIIOCMessage (n :: Nat) = SPIIOCMessage Raw.SPIIOCCommand
  deriving (Eq, Ord)

mkSPIIOCMessage :: (KnownNat n, CmpNat n 512 ~ LT)
                => Proxy n -> SPIIOCMessage n
mkSPIIOCMessage p = do
  let CUChar magicW8 = Raw.SPI_IOC_MAGIC
  SPIIOCMessage $ Raw.SPIIOCCommand $
    IOW magicW8 0 (fromIntegral $ spiMessageSize' p)

spiMessageSize' :: (KnownNat n, CmpNat n 512 ~ LT)
                => Proxy n -> Integer
spiMessageSize' n =
  natVal n * fromIntegral (sizeOf (undefined :: Raw.SPIIOCTransfer))

spiMessageSize :: Integer -> Maybe Integer
spiMessageSize n = do
  let transferSize = n * fromIntegral (sizeOf (undefined :: Raw.SPIIOCTransfer))
  guard $ transferSize < (1 `shiftL` fromIntegral Raw.IOC_SIZEBITS)
  pure transferSize

newtype SPIExcept a =
  SPIExcept { runSPIExcept :: IO (Either SPIError a) }

instance Functor SPIExcept where
  fmap f (SPIExcept a) = SPIExcept $ fmap (fmap f) a

instance Applicative SPIExcept where
  pure = SPIExcept . pure . pure
  SPIExcept f <*> SPIExcept x = SPIExcept $ do
    f' <- f
    case f' of
      Left errno -> pure $ Left errno
      Right fr -> do
        x' <- x
        case x' of
          Left errno -> pure $ Left errno
          Right xr -> pure $ Right $ fr xr

instance Monad SPIExcept where
  return = pure
  SPIExcept x >>= f = SPIExcept $ do
    x' <- x
    case x' of
      Left errno -> pure $ Left errno
      Right xr -> do
        let SPIExcept f' = f xr
        f'

fromIO :: IO a -> SPIExcept a
fromIO = SPIExcept . fmap Right

fdGetMode :: FD -> IO (Either SPIError Mode)
fdGetMode fd =
  alloca $ \modePtr -> runSPIExcept $ do
    exceptErrnoIfMinus1Retry_ "fdGetMode" $
      Raw.ioctl (fdFD fd) Raw.SPI_IOC_RD_MODE modePtr
    word8Mode <$> fromIO (peek modePtr)

fdSetMode :: FD -> Mode -> IO (Either SPIError ())
fdSetMode fd m =
  alloca $ \modePtr -> runSPIExcept $ do
    fromIO $ poke modePtr $ modeWord8 m
    exceptErrnoIfMinus1Retry_ "fdSetMode" $
      Raw.ioctl (fdFD fd) Raw.SPI_IOC_WR_MODE modePtr

fdGetMaxSpeedHz :: FD -> IO (Either SPIError MaxSpeedHz)
fdGetMaxSpeedHz fd =
  alloca $ \maxSpeedHzPtr -> runSPIExcept $ do
    exceptErrnoIfMinus1Retry_ "fdGetMaxSpeedHz" $
      Raw.ioctl (fdFD fd) Raw.SPI_IOC_RD_MAX_SPEED_HZ maxSpeedHzPtr
    fmap MaxSpeedHz $ fromIO $ peek maxSpeedHzPtr

fdSetMaxSpeedHz :: FD -> MaxSpeedHz -> IO (Either SPIError ())
fdSetMaxSpeedHz fd (MaxSpeedHz maxSpeedHz) =
  alloca $ \maxSpeedHzPtr -> runSPIExcept $ do
    fromIO $ poke maxSpeedHzPtr maxSpeedHz
    exceptErrnoIfMinus1Retry_ "fdSetMaxSpeedHz" $
      Raw.ioctl (fdFD fd) Raw.SPI_IOC_WR_MAX_SPEED_HZ maxSpeedHzPtr

fdGetBitsPerWord :: FD -> IO (Either SPIError BitsPerWord)
fdGetBitsPerWord fd =
  alloca $ \bitsPerWordPtr -> runSPIExcept $ do
    exceptErrnoIfMinus1Retry_ "fdGetBitsPerWord" $
      Raw.ioctl (fdFD fd) Raw.SPI_IOC_RD_BITS_PER_WORD bitsPerWordPtr
    fmap BitsPerWord $ fromIO $ peek bitsPerWordPtr

fdSetBitsPerWord :: FD -> BitsPerWord -> IO (Either SPIError ())
fdSetBitsPerWord fd (BitsPerWord bpw) =
  alloca $ \bitsPerWordPtr -> runSPIExcept $ do
    fromIO $ poke bitsPerWordPtr bpw
    exceptErrnoIfMinus1Retry_ "fdSetBitsPerWord" $
      Raw.ioctl (fdFD fd) Raw.SPI_IOC_WR_BITS_PER_WORD bitsPerWordPtr

data SPIError
  = SPIIOError IOError
  deriving (Show)

instance Exception SPIError where

exceptErrnoIfMinus1Retry :: (Eq a, Num a) => String -> IO a -> SPIExcept a
exceptErrnoIfMinus1Retry loc f = do
  res <- fromIO f
  case res of
    -1 -> do
      err <- fromIO getErrno
      if err == eINTR
        then exceptErrnoIfMinus1Retry loc f
        else do
          SPIExcept $ pure $ Left $ SPIIOError $
            errnoToIOError loc err Nothing Nothing
    other -> pure other

exceptErrnoIfMinus1Retry_ :: (Eq a, Num a) => String -> IO a -> SPIExcept ()
exceptErrnoIfMinus1Retry_ loc f =
  void $ exceptErrnoIfMinus1Retry loc f

castPtrWord64 :: Ptr a -> Word64
castPtrWord64 = fromIntegral . ptrToWordPtr

calloca :: Storable a => (Ptr a -> IO b) -> IO b
calloca = bracket calloc free
