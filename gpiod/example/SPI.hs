module SPI where

import Control.Monad.Trans.State
import Control.Concurrent.MVar
import Control.Exception
import Data.Bits
import Data.Word
import Foreign.C.Error
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.IO.FD (FD)
import qualified GHC.IO.Device as Device
import qualified GHC.IO.FD as FD
import Data.Proxy
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

import qualified SPIDev

data SPIMode = SPIMode
  { cpha :: Bool
  , cpol :: Bool
  , csHigh :: Bool
  , lsbFirst :: Bool
  , threeWire :: Bool
  , loop :: Bool
  , noCS :: Bool
  , ready :: Bool
  } deriving (Show, Eq, Ord)

word8SPIMode :: Word8 -> SPIMode
word8SPIMode w = flip evalState w $
  SPIMode <$> shuntBit
          <*> shuntBit
          <*> shuntBit
          <*> shuntBit
          <*> shuntBit
          <*> shuntBit
          <*> shuntBit
          <*> shuntBit
  where
    shuntBit :: State Word8 Bool
    shuntBit = state \b -> (b `testBit` 0, b `shiftR` 1)

data SPI = SPI
  { spiMode :: MVar SPIDev.SPIMode
  , spiMaxClockSpeedHz :: MVar Word32
  , spiFD :: FD
  }

mkSPI :: FD -> IO SPI
mkSPI fd = do
  mode <- getMode fd >>= newMVar
  hz <- getMaxClockSpeedHz fd >>= newMVar
  pure SPI
    { spiMode = mode
    , spiMaxClockSpeedHz = hz
    , spiFD = fd
    }

getSPIMode :: SPI -> IO SPIMode
getSPIMode spi = do
  SPIDev.SPIMode w8 <- readMVar $ spiMode spi
  pure $ word8SPIMode w8

getMode :: FD -> IO SPIDev.SPIMode
getMode fd = alloca $ \modePtr -> do
  0 <- throwErrnoIfMinus1 "ioctl SPI_IOC_RD_MODE" $
    SPIDev.ioctl (FD.fdFD fd) SPIDev.SPI_IOC_RD_MODE modePtr
  peek modePtr

getMaxClockSpeedHz :: FD -> IO Word32
getMaxClockSpeedHz fd = alloca $ \hzPtr -> do
  0 <- throwErrnoIfMinus1 "ioctl SPI_IOC_RD_MODE" $
    SPIDev.ioctl (FD.fdFD fd) SPIDev.SPI_IOC_RD_MAX_SPEED_HZ hzPtr
  peek hzPtr

setNoCS :: Bool -> SPI -> IO ()
setNoCS newNoCS spi = modifyMVar_ (spiMode spi) $ \m -> do
  let newM =
        if newNoCS
          then m .|. SPIDev.SPI_NO_CS
          else m .&. complement SPIDev.SPI_NO_CS
  res <- alloca $ \modePtr -> do
    poke modePtr newM
    0 <- throwErrnoIfMinus1 "ioctl SPI_IOC_WR_MODE" $
      SPIDev.ioctl (FD.fdFD (spiFD spi)) SPIDev.SPI_IOC_WR_MODE modePtr
    peek modePtr
  assert (res == newM) $ pure newM

setThreeWire :: Bool -> SPI -> IO ()
setThreeWire new3Wire spi = modifyMVar_ (spiMode spi) $ \m -> do
  let newM =
        if new3Wire
          then m .|. SPIDev.SPI_3WIRE
          else m .&. complement SPIDev.SPI_3WIRE
  res <- alloca $ \modePtr -> do
    poke modePtr newM
    0 <- throwErrnoIfMinus1 "ioctl SPI_IOC_WR_MODE" $
      SPIDev.ioctl (FD.fdFD (spiFD spi)) SPIDev.SPI_IOC_WR_MODE modePtr
    peek modePtr
  assert (res == newM) $ pure newM

setMaxClockHz :: Word32 -> SPI -> IO ()
setMaxClockHz hz spi = do
  alloca $ \hzPtr -> do
    poke hzPtr hz
    0 <- throwErrnoIfMinus1 "ioctl SPI_IOC_WR_MAX_SPEED_HZ" $
      SPIDev.ioctl (FD.fdFD (spiFD spi)) SPIDev.SPI_IOC_WR_MAX_SPEED_HZ hzPtr
    pure ()

readByte :: SPI -> IO Word8
readByte spi =
  calloca $ \wptr -> do
    -- poke wptr 127
    calloca $ \tptr -> do
      poke tptr SPIDev.SPIIOCTransfer
        { txBuf = castPtrWord64 nullPtr
        , rxBuf = castPtrWord64 wptr

        , len = fromIntegral $ sizeOf (undefined :: Word8)
        , speedHz = 0

        , delayUsecs = 0
        , bitsPerWord = 0
        , txNBits = 0
        , rxNBits = 0
        , wordDelayUsecs = 0
        , csChange = 0
        , pad = 0
        }
      let SPIDev.SPIIOCMessage cmd =
            SPIDev.mkSPIIOCMessage (Proxy :: Proxy 1)
      1 <- throwErrnoIfMinus1 "ioctl SPI_IOC_MESSAGE" $
        SPIDev.ioctl (FD.fdFD (spiFD spi)) cmd tptr
      peek wptr

writeByte :: Word8 -> SPI -> IO ()
writeByte w spi = writeBytes (ByteString.singleton w) spi

readBytes :: Int -> SPI -> IO ByteString
readBytes 0 _ = pure ""
readBytes n spi = do
  allocaBytes n $ \str -> do
    -- r <- Device.read (spiFD spi) str 0 n
    -- assert (r == n) $
    --   ByteString.packCStringLen (castPtr str, n)
    calloca $ \tptr -> do
      poke tptr SPIDev.SPIIOCTransfer
        { txBuf = castPtrWord64 nullPtr
        , rxBuf = castPtrWord64 str

        , len = fromIntegral n
        , speedHz = 0

        , delayUsecs = 0
        , bitsPerWord = 0
        , txNBits = 0
        , rxNBits = 0
        , wordDelayUsecs = 0
        , csChange = 0
        , pad = 0
        }
      let SPIDev.SPIIOCMessage cmd =
            SPIDev.mkSPIIOCMessage (Proxy :: Proxy 1)
      _ <- throwErrnoIfMinus1 "ioctl SPI_IOC_MESSAGE" $
        SPIDev.ioctl (FD.fdFD (spiFD spi)) cmd tptr
      ByteString.packCStringLen (str, n)

writeBytes :: ByteString -> SPI -> IO ()
writeBytes "" _ = pure ()
writeBytes b spi =
  ByteString.useAsCStringLen b $ \(str, l) -> do
    -- Device.write (spiFD spi) (castPtr str) 0 l
    calloca $ \tptr -> do
      poke tptr SPIDev.SPIIOCTransfer
        { txBuf = castPtrWord64 str
        , rxBuf = castPtrWord64 nullPtr

        , len = fromIntegral l
        , speedHz = 0

        , delayUsecs = 0
        , bitsPerWord = 0
        , txNBits = 0
        , rxNBits = 0
        , wordDelayUsecs = 0
        , csChange = 0
        , pad = 0
        }
      let SPIDev.SPIIOCMessage cmd =
            SPIDev.mkSPIIOCMessage (Proxy :: Proxy 1)
      _ <- throwErrnoIfMinus1 "ioctl SPI_IOC_MESSAGE" $
        SPIDev.ioctl (FD.fdFD (spiFD spi)) cmd tptr
      pure ()

castPtrWord64 :: Ptr a -> Word64
castPtrWord64 = fromIntegral . ptrToWordPtr

calloca :: Storable a => (Ptr a -> IO b) -> IO b
calloca = bracket calloc free
