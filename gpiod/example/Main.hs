module Main where

import Codec.Picture
import Codec.Picture.Types
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.ByteString (ByteString)
import Data.Char
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Proxy
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word
import GHC.TypeLits
import Numeric
import System.IO
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Vector as V
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable as SV
import qualified GHC.IO.FD as FD

import BitVec
import SPI (SPI)
import System.IO.GPIOD.Chip
import System.IO.GPIOD.Line (Line, lookupChipLineByNumber, lookupChipLineByName)
import System.IO.GPIOD.LineEvent (LineEvent)
import System.IO.GPIOD.LineRequest
import System.IO.GPIOD.ReservedLine
import qualified SPI
import qualified System.IO.GPIOD.LineEvent as LineEvent

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  putStrLn "getting all chip information"
  gpiochip0 <- lookupChipByNumber 0 >>= \case
    Left err -> error $ show err
    Right Nothing -> error $ "Couldn't find gpiochip0!"
    Right (Just c) -> pure c

  resetL <- lookupChipLineByNumberFail gpiochip0 resetPin
  busyL <- lookupChipLineByNumberFail gpiochip0 busyPin
  dataCommandL <- lookupChipLineByNumberFail gpiochip0 dataCommandPin
  csL <- lookupChipLineByNumberFail gpiochip0 csPin

  withReservedLineFail resetL resetRequest $ \reset ->
    withReservedLineFail busyL busyRequest $ \busy ->
      withReservedLineFail dataCommandL dataCommandRequest $ \dataCommand ->
        withReservedLineFail csL csRequest $ \cs -> do
          withSpidev $ \spi -> do
            -- set NO_CS mode to False
            SPI.setNoCS False spi

            -- set MAX_SPEED_HZ to 3000000
            SPI.setMaxClockHz 3_000_000 spi

            SPI.setThreeWire True spi

            SPI.getSPIMode spi >>= print

            (eh, isBusy, mkTChan) <- newBusyBroadcastTChan busy
            tchan <- atomically mkTChan
            let d = Display
                  { resetLine = reset
                  , busyLine = busy
                  , wait = waitBusyTChan tchan
                  , busyState = do
                      p <- eventHandlerHasPendingEvents eh
                      if p
                        then retry
                        else
                          isBusy <&> \case
                            Low -> True
                            High -> False
                  , dataCommandLine = dataCommand
                  , csLine = cs
                  , spiBus = spi
                  }
            mainLoop d

mainLoop :: Display -> IO ()
mainLoop disp = do
  putStrLn "reading image"
  pixelBS <- loadImageAsPixelByteString
  print $ ByteString.length pixelBS
  let expectedImageLength = fromIntegral (fst resolution) * fromIntegral (snd resolution) `div` 2
  unless (ByteString.length pixelBS == expectedImageLength) $
    error $ "wrong length image ByteString!: " <> show (ByteString.length pixelBS) <> ", expected " <> show expectedImageLength

  putStrLn "reset -> low"
  setValue (resetLine disp) Low >>= either throwIO pure
  threadDelay 100_000

  putStrLn "reset -> high"
  setValue (resetLine disp) High >>= either throwIO pure
  threadDelay 1_000_000

  let dig2char d = chr (ord '0' + fromIntegral d)

  putStrLn "TRES"
  _ <- sendCommand disp TRES $
    NByteString $ ByteString.Lazy.toStrict $ Builder.toLazyByteString $ mconcat
      [ Builder.word16BE (fst resolution)
      , Builder.word16BE (snd resolution)
      ]

  putStrLn "PSR"
  _ <- sendCommand disp PSR do
    let b1 =
          fromWord @2 0b10
            `append` fromBool True -- TODO: unknown? not mentioned in docs
            `append` fromBool False -- Empty
            `append` fromBool True -- Scan direction -- down: 0, up: 1
            `append` fromBool True -- Shift direction -- left: 0, right: 1
            `append` fromBool True -- Booster switch -- dc-dc off: 0, dc-dc on:
            `append` fromBool True -- Reset mode -- soft: 0, hard: 1
    NByteString $ ByteString.pack [bitVecWord8 b1, 0x08]

  putStrLn "PWR"
  _ <- sendCommand disp PWR do
    let b1 =
          fromBool True
            `append` fromBool True
            `append` fromBool False
            `append` fromBool True
            `append` fromBool True
            `append` fromBool True
        b2 =
          fromWord @2 0b00
        b3 = 0x23 -- 10V
        b4 = 0x23 -- 10V
    NByteString $ ByteString.pack [bitVecWord8 b1, bitVecWord8 b2, b3, b4]

  putStrLn "PLL"
  _ <- sendCommand disp PLL $
    NByteString $ ByteString.singleton $ bitVecWord8 $ fromWord @3 7 `append` fromWord @3 4

  putStrLn "TSE"
  _ <- sendCommand disp TSE $ NByteString $ ByteString.singleton 0

  putStrLn "TSR"
  NByteString tsrBS <- sendCommand disp TSR (NByteString mempty)
  print $ ByteString.unpack tsrBS

  putStrLn "CDI"
  _ <- sendCommand disp CDI do
    let b1 =
          fromWord8 @3 (colorWord8 White)
            `append` fromBool True -- Data polarity
            `append` fromWord @4 0b0111
    NByteString $ ByteString.singleton $ bitVecWord8 b1

  putStrLn "TCON"
  _ <- sendCommand disp TCON $
    NByteString $ ByteString.singleton $ bitVecWord8 $ fromWord @4 0b0010 `append` fromWord @4 0b0010

  putStrLn "DAM"
  _ <- sendCommand disp DAM $ NByteString $ ByteString.singleton 0

  putStrLn "PWS"
  _ <- sendCommand disp PWS $ NByteString $ ByteString.singleton 0xAA --0b10101010

  putStrLn "PFS"
  _ <- sendCommand disp PFS $ NByteString $ ByteString.singleton 0

  putStrLn "DTM1"
  _ <- sendCommand disp DTM1 $ NByteString @128000 pixelBS
  _ <- sendCommand disp DTM1 $ NByteString @256000 (pixelBS <> pixelBS)
  -- _ <- sendCommand disp DTM1 $ NByteString @128000 $ ByteString.replicate 128000 0b00010001
  -- threadDelay 1_000_000

  -- putStrLn "DSP"
  -- NByteString dspBS <- sendCommand disp DSP (NByteString mempty)
  -- print $ fmap (\w -> showIntAtBase 2 dig2char w "") $ ByteString.unpack dspBS

  putStrLn "FLG"
  NByteString flgBS <- sendCommand disp FLG (NByteString mempty)
  print $ fmap (\w -> showIntAtBase 2 dig2char w "") $ ByteString.unpack flgBS

  putStrLn "PON"
  _ <- sendCommand disp PON $ NByteString ByteString.empty
  threadDelay 200_000

  putStrLn "DRF"
  _ <- sendCommand disp DRF $ NByteString ByteString.empty
  threadDelay 32_000_000

  putStrLn "POF"
  _ <- sendCommand disp POF $ NByteString ByteString.empty
  threadDelay 200_000

  pure ()

loadImageAsPixelByteString :: IO ByteString
loadImageAsPixelByteString = do
  bs <- ByteString.readFile "haskell-png.png"
  let img = case decodePng bs of
        Left err -> error err
        Right dynamicImage -> case dynamicImage of
          ImageY8 i -> extractLumaPlane i
          ImageRGB8 i -> extractLumaPlane i
          ImageRGBA8 i -> extractLumaPlane i
          _ -> error "Unexpected pixel type"
  pure $ ByteString.Lazy.toStrict $ Builder.toLazyByteString $ foldMap Builder.word8 $ runST do
    let imgLen = SV.length $ imageData img
    mv <- MV.replicate (imgLen `div` 2) 0
    SV.iforM_ (imageData img) \ix p -> do
      let targetIx = (imgLen - ix) - 1
      let s = if odd ix then 4 else 0
          v = colorWord8 $ if dist minBound p > dist maxBound p then White else Black
      MV.modify mv (.|. (v `shiftL` s)) (targetIx `div` 2)
    V.freeze mv

dist :: Word8 -> Word8 -> Word8
dist a b = if a > b then abs (a - b) else abs (b - a)

foldOr :: Bits a => [a] -> a
foldOr = foldl' (.|.) zeroBits

resetPin :: Word
resetPin = 27 -- PIN13

busyPin :: Word
busyPin = 17 -- PIN11

dataCommandPin :: Word
dataCommandPin = 22 -- PIN15

csPin :: Word
csPin = 8

spiClockPin :: Word
spiClockPin = 11

buttonAPin :: Word
buttonAPin = 5 -- GPIO5

buttonBPin :: Word
buttonBPin = 6 -- GPIO6

buttonCPin :: Text
buttonCPin = "GPIO16"

buttonDPin :: Text
buttonDPin = "GPIO24"

-- ["PIN29", "PIN31", "PIN36", "PIN18"]

resolution :: (Word16, Word16)
resolution = (640, 400)

resolutionMagic :: Word8
resolutionMagic = 0b10

lookupChipLineByNumberFail :: Chip -> Word -> IO Line
lookupChipLineByNumberFail c o =
  lookupChipLineByNumber c o >>= \case
    Left err -> throwIO err
    Right Nothing -> error $
      "Couldn't find line " <> show o
    Right (Just l) -> pure l

lookupChipLineByNameFail :: Chip -> Text -> IO Line
lookupChipLineByNameFail c n =
  lookupChipLineByName c n >>= \case
    Left err -> throwIO err
    Right Nothing -> error $
      "Couldn't find line " <> show n
    Right (Just l) -> pure l

withReservedLineFail :: KnownRequestType rt
                     => Line
                     -> LineRequest rt
                     -> (ReservedLine rt -> IO a)
                     -> IO a
withReservedLineFail l r f =
  withReservedLine l r f >>= \case
    Left err -> throwIO err
    Right v -> pure v

resetRequest :: LineRequest 'Output
resetRequest = outputRequest "gpiod-example" High
  & setBias (Just Disable)

busyRequest :: LineRequest ('Events 'BothEdges)
busyRequest = eventsRequest "gpiod-example"
  & setBias (Just Disable)

dataCommandRequest :: LineRequest 'Output
dataCommandRequest = outputRequest "gpiod-example" Low
  & setBias (Just Disable)

csRequest :: LineRequest 'Output
csRequest = outputRequest "gpiod-example" High
  & setBias (Just Disable)

spiClockRequest :: LineRequest ('Events 'BothEdges)
spiClockRequest = eventsRequest "gpiod-example"
  & setBias (Just Disable)

buttonRequest :: LineRequest ('Events 'BothEdges)
buttonRequest = eventsRequest "gpiod-example"
  & setBias (Just PullUp)

withSpidev :: (SPI -> IO a) -> IO a
withSpidev f =
  bracket
    (FD.openFileWith "/dev/spidev0.0" ReadWriteMode True (\fd _ -> pure fd) (\_ fd -> pure fd))
    FD.release
    (\fd -> SPI.mkSPI fd >>= f)

data Display = Display
  { resetLine :: ReservedLine 'Output
  , busyLine :: ReservedLine ('Events 'BothEdges)
  , wait :: STM ()
  , busyState :: STM Bool
  , dataCommandLine :: ReservedLine 'Output
  , csLine :: ReservedLine 'Output
  , spiBus :: SPI
  }

data Color
  = Black
  | White
  | Green
  | Blue
  | Red
  | Yellow
  | Orange
  | Clear
  deriving (Show, Eq, Ord, Enum, Bounded)

colorWord8 :: Color -> Word8
colorWord8 = \case
  Black -> 0
  White -> 1
  Green -> 2
  Blue -> 3
  Red -> 4
  Yellow -> 5
  Orange -> 6
  Clear -> 7

newBusyBroadcastTChan :: ReservedLine ('Events 'BothEdges)
                      -> IO (EventHandler, STM ActiveState, STM (TChan LineEvent))
newBusyBroadcastTChan rl = do
  tchan <- newBroadcastTChanIO
  initialValue <- getValue rl >>= either throwIO pure
  tvar <- newTVarIO initialValue
  let write = do
        either throwIO $ \e -> do
          putStrLn $ "busy: " <> show (LineEvent.eventType e)
          atomically do
            writeTChan tchan e
            writeTVar tvar $ case LineEvent.eventType e of
              LineEvent.RisingEdge -> High
              LineEvent.FallingEdge -> Low
  eh <- watchLineEvents write rl
  pure (eh, readTVar tvar, dupTChan tchan)

waitBusyTChan :: TChan LineEvent -> STM ()
waitBusyTChan c = do
  e <- readTChan c
  case LineEvent.eventType e of
    LineEvent.RisingEdge -> pure ()
    LineEvent.FallingEdge -> waitBusyTChan c

data UC8159Command (b :: Nat) (w :: Nat) (r :: Nat) where
  PSR  :: UC8159Command 0x00 2 0
  PWR  :: UC8159Command 0x01 4 0
  POF  :: UC8159Command 0x02 0 0
  PFS  :: UC8159Command 0x03 1 0
  PON  :: UC8159Command 0x04 0 0
  DTM1 :: UC8159Command 0x10 n 0
  DSP  :: UC8159Command 0x11 0 1
  DRF  :: UC8159Command 0x12 0 0
  IPC  :: UC8159Command 0x13 1 0
  PLL  :: UC8159Command 0x30 1 0
  TSE  :: UC8159Command 0x41 1 0
  TSR  :: UC8159Command 0x43 0 2
  CDI  :: UC8159Command 0x50 1 0
  TCON :: UC8159Command 0x60 1 0
  TRES :: UC8159Command 0x61 4 0
  DAM  :: UC8159Command 0x65 1 0
  REV  :: UC8159Command 0x70 0 2
  FLG  :: UC8159Command 0x71 0 1
  PWS  :: UC8159Command 0xE3 1 0

-- UC8159_PSR = 0x00
-- UC8159_PWR = 0x01
-- UC8159_POF = 0x02
-- UC8159_PFS = 0x03
-- UC8159_PON = 0x04
-- UC8159_BTST = 0x06
-- UC8159_DSLP = 0x07
-- UC8159_DTM1 = 0x10
-- UC8159_DSP = 0x11
-- UC8159_DRF = 0x12
-- UC8159_IPC = 0x13
-- UC8159_PLL = 0x30
-- UC8159_TSC = 0x40
-- UC8159_TSE = 0x41
-- UC8159_TSW = 0x42
-- UC8159_TSR = 0x43
-- UC8159_CDI = 0x50
-- UC8159_LPD = 0x51
-- UC8159_TCON = 0x60
-- UC8159_TRES = 0x61
-- UC8159_DAM = 0x65
-- UC8159_REV = 0x70
-- UC8159_FLG = 0x71
-- UC8159_AMV = 0x80
-- UC8159_VV = 0x81
-- UC8159_VDCS = 0x82
-- UC8159_PWS = 0xE3
-- UC8159_TSSET = 0xE5

sendCommand :: (KnownNat n, CmpNat n 256 ~ LT, KnownNat w, KnownNat r)
            => Display
            -> UC8159Command n w r
            -> NByteString w
            -> IO (NByteString r)
sendCommand d (_c :: UC8159Command n w r) (NByteString i) = do
  let n = word8NatVal (Proxy :: Proxy n)
      -- w = natVal (Proxy :: Proxy w)
      r = natVal (Proxy :: Proxy r)

  setValue (csLine d) Low >>= either throwIO pure

  waitUntilNotBusy d

  setValue (dataCommandLine d) Low >>= either throwIO pure
  SPI.writeByte n (spiBus d)

  setValue (dataCommandLine d) High >>= either throwIO pure
  goWrite i

  ret <- NByteString <$> SPI.readBytes (fromIntegral r) (spiBus d)

  waitUntilNotBusy d

  setValue (csLine d) High >>= either throwIO pure

  pure ret
    where
      goWrite b =
        if ByteString.null b
          then pure ()
          else do
            let (h, t) = ByteString.splitAt 512 b
            SPI.writeBytes h (spiBus d)
            goWrite t

word8NatVal :: (KnownNat n, CmpNat n 256 ~ LT) => Proxy n -> Word8
word8NatVal p = fromIntegral (natVal p)

data Resolution = Resolution Word16 Word16
  deriving (Show, Eq, Ord)

newtype Vec (n :: Nat) a =
  Vec (Vector a)
  deriving (Show, Eq, Ord)

newtype NByteString (n :: Nat) =
  NByteString ByteString
  deriving (Show, Eq, Ord)

unsafeVec :: Vector a -> Vec n a
unsafeVec = Vec

fromVector :: KnownNat n => Proxy n -> Vector a -> Maybe (Vec n a)
fromVector p v = do
  guard $ natVal p == fromIntegral (Vector.length v)
  pure $ Vec v

waitUntilNotBusy :: Display -> IO ()
waitUntilNotBusy d = do
  yield
  atomically do
    busyState d >>= \case
      True -> retry
      False -> pure ()
