module SPIDev
  ( SPIMode(..)
  , pattern SPI_CPHA
  , pattern SPI_CPOL
  , pattern SPI_MODE_0
  , pattern SPI_MODE_1
  , pattern SPI_MODE_2
  , pattern SPI_MODE_3
  , pattern SPI_CS_HIGH
  , pattern SPI_LSB_FIRST
  , pattern SPI_3WIRE
  , pattern SPI_LOOP
  , pattern SPI_NO_CS
  , pattern SPI_READY
  , pattern SPI_IOC_MAGIC
  , spiMessageSize
  , SPIIOCTransfer(..)
  , SPIIOCMessage(..)
  , mkSPIIOCMessage
  , pattern SPI_IOC_RD_MODE
  , pattern SPI_IOC_WR_MODE
  , pattern SPI_IOC_RD_LSB_FIRST
  , pattern SPI_IOC_WR_LSB_FIRST
  , pattern SPI_IOC_RD_BITS_PER_WORD
  , pattern SPI_IOC_WR_BITS_PER_WORD
  , pattern SPI_IOC_RD_MAX_SPEED_HZ
  , pattern SPI_IOC_WR_MAX_SPEED_HZ
  , ioctl
  ) where

import Control.Monad
import Data.Bits
import Data.Proxy
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.TypeLits

import qualified IOCtl.Raw as IOCtl

#include "linux/spi/spidev.h"

newtype SPIMode = SPIMode Word8
  deriving (Show, Eq, Ord, Bits, Storable)

pattern SPI_CPHA :: SPIMode
pattern SPI_CPHA = SPIMode #{const SPI_CPHA}

pattern SPI_CPOL :: SPIMode
pattern SPI_CPOL = SPIMode #{const SPI_CPOL}

pattern SPI_MODE_0 :: SPIMode
pattern SPI_MODE_0 = SPIMode #{const SPI_MODE_0}

pattern SPI_MODE_1 :: SPIMode
pattern SPI_MODE_1 = SPIMode #{const SPI_MODE_1}

pattern SPI_MODE_2 :: SPIMode
pattern SPI_MODE_2 = SPIMode #{const SPI_MODE_2}

pattern SPI_MODE_3 :: SPIMode
pattern SPI_MODE_3 = SPIMode #{const SPI_MODE_3}

pattern SPI_CS_HIGH :: SPIMode
pattern SPI_CS_HIGH = SPIMode #{const SPI_CS_HIGH}

pattern SPI_LSB_FIRST :: SPIMode
pattern SPI_LSB_FIRST = SPIMode #{const SPI_LSB_FIRST}

pattern SPI_3WIRE :: SPIMode
pattern SPI_3WIRE = SPIMode #{const SPI_3WIRE}

pattern SPI_LOOP :: SPIMode
pattern SPI_LOOP = SPIMode #{const SPI_LOOP}

pattern SPI_NO_CS :: SPIMode
pattern SPI_NO_CS = SPIMode #{const SPI_NO_CS}

pattern SPI_READY :: SPIMode
pattern SPI_READY = SPIMode #{const SPI_READY}

pattern SPI_IOC_MAGIC :: CUChar
pattern SPI_IOC_MAGIC = #{const SPI_IOC_MAGIC}

pattern IOC_SIZEBITS :: CSize
pattern IOC_SIZEBITS = #{const _IOC_SIZEBITS}

spiMessageSize :: Integer -> Maybe Integer
spiMessageSize n = do
  let transferSize = n * fromIntegral (sizeOf (undefined :: SPIIOCTransfer))
  guard $ transferSize < (1 `shiftL` fromIntegral IOC_SIZEBITS)
  pure transferSize

data SPIIOCTransfer = SPIIOCTransfer
  { txBuf :: Word64
  , rxBuf :: Word64

  , len :: Word32
  , speedHz :: Word32

  , delayUsecs :: Word16
  , bitsPerWord :: Word8
  , csChange :: Word8
  , txNBits :: Word8
  , rxNBits :: Word8
  , wordDelayUsecs :: Word8
  , pad :: Word8
  } deriving (Show, Eq, Ord)

instance Storable SPIIOCTransfer where
  sizeOf _ = #{size struct spi_ioc_transfer}
  alignment _ = #{alignment struct spi_ioc_transfer}
  peek p =
    SPIIOCTransfer <$> #{peek struct spi_ioc_transfer, tx_buf} p
                   <*> #{peek struct spi_ioc_transfer, rx_buf} p

                   <*> #{peek struct spi_ioc_transfer, len} p
                   <*> #{peek struct spi_ioc_transfer, speed_hz} p

                   <*> #{peek struct spi_ioc_transfer, delay_usecs} p
                   <*> #{peek struct spi_ioc_transfer, bits_per_word} p
                   <*> #{peek struct spi_ioc_transfer, cs_change} p
                   <*> #{peek struct spi_ioc_transfer, tx_nbits} p
                   <*> #{peek struct spi_ioc_transfer, rx_nbits} p
                   <*> #{peek struct spi_ioc_transfer, word_delay_usecs} p
                   <*> #{peek struct spi_ioc_transfer, pad} p
  poke p t = do
    #{poke struct spi_ioc_transfer, tx_buf} p (txBuf t)
    #{poke struct spi_ioc_transfer, rx_buf} p (rxBuf t)

    #{poke struct spi_ioc_transfer, len} p (len t)
    #{poke struct spi_ioc_transfer, speed_hz} p (speedHz t)

    #{poke struct spi_ioc_transfer, delay_usecs} p (delayUsecs t)
    #{poke struct spi_ioc_transfer, bits_per_word} p (bitsPerWord t)
    #{poke struct spi_ioc_transfer, cs_change} p (csChange t)
    #{poke struct spi_ioc_transfer, pad} p (pad t)

newtype SPIIOCCommand = SPIIOCCommand CULong
  deriving (Eq, Ord)

newtype SPIIOCMessage (n :: Nat) = SPIIOCMessage SPIIOCCommand
  deriving (Eq, Ord)

-- class (KnownNat n, CmpNat n 512 ~ LT) => SPIIOCMessageSize n where
-- instance (KnownNat n, CmpNat n 512 ~ LT) => SPIIOCMessageSize n where

mkSPIIOCMessage :: (KnownNat n, CmpNat n 512 ~ LT)
                => Proxy n -> SPIIOCMessage n
mkSPIIOCMessage p = do
  let CUChar magicW8 = SPI_IOC_MAGIC
  SPIIOCMessage $ SPIIOCCommand $
    IOCtl.IOW magicW8 0 (fromIntegral $ spiMessageSize' p)

spiMessageSize' :: (KnownNat n, CmpNat n 512 ~ LT)
                => Proxy n -> Integer
spiMessageSize' n =
  natVal n * fromIntegral (sizeOf (undefined :: SPIIOCTransfer))

pattern SPI_IOC_RD_MODE :: SPIIOCCommand
pattern SPI_IOC_RD_MODE = SPIIOCCommand #{const SPI_IOC_RD_MODE}

pattern SPI_IOC_WR_MODE :: SPIIOCCommand
pattern SPI_IOC_WR_MODE = SPIIOCCommand #{const SPI_IOC_WR_MODE}

pattern SPI_IOC_RD_LSB_FIRST :: SPIIOCCommand
pattern SPI_IOC_RD_LSB_FIRST = SPIIOCCommand #{const SPI_IOC_RD_LSB_FIRST}

pattern SPI_IOC_WR_LSB_FIRST :: SPIIOCCommand
pattern SPI_IOC_WR_LSB_FIRST = SPIIOCCommand #{const SPI_IOC_WR_LSB_FIRST}

pattern SPI_IOC_RD_BITS_PER_WORD :: SPIIOCCommand
pattern SPI_IOC_RD_BITS_PER_WORD = SPIIOCCommand #{const SPI_IOC_RD_BITS_PER_WORD}

pattern SPI_IOC_WR_BITS_PER_WORD :: SPIIOCCommand
pattern SPI_IOC_WR_BITS_PER_WORD = SPIIOCCommand #{const SPI_IOC_WR_BITS_PER_WORD}

pattern SPI_IOC_RD_MAX_SPEED_HZ :: SPIIOCCommand
pattern SPI_IOC_RD_MAX_SPEED_HZ = SPIIOCCommand #{const SPI_IOC_RD_MAX_SPEED_HZ}

pattern SPI_IOC_WR_MAX_SPEED_HZ :: SPIIOCCommand
pattern SPI_IOC_WR_MAX_SPEED_HZ = SPIIOCCommand #{const SPI_IOC_WR_MAX_SPEED_HZ}


foreign import capi "sys/ioctl.h"
  ioctl :: CInt -> SPIIOCCommand -> Ptr a -> IO CInt
