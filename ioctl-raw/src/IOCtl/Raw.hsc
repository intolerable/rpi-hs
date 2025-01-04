module IOCtl.Raw
  ( pattern IOC_NRBITS
  , pattern IOC_TYPEBITS
  , pattern IOC_SIZEBITS
  , pattern IOC_DIRBITS
  , pattern IOC_NRMASK
  , pattern IOC_TYPEMASK
  , pattern IOC_SIZEMASK
  , pattern IOC_DIRMASK
  , pattern IOC_NRSHIFT
  , pattern IOC_TYPESHIFT
  , pattern IOC_SIZESHIFT
  , pattern IOC_DIRSHIFT
  , pattern IOC_NONE
  , pattern IOC_WRITE
  , pattern IOC_READ
  , pattern IOC_WRITEREAD
  , pattern IOC
  , pattern IOR
  , pattern IOW
  , pattern IOWR
  , module IOCtl.Raw
  ) where

import Data.Bits
import Data.Word
import Debug.Trace
import Foreign.C.Types
import Foreign.Ptr
import Unsafe.Coerce

#include "sys/ioctl.h"

pattern IOC_NRBITS :: CULong
pattern IOC_NRBITS = #{const _IOC_NRBITS}

pattern IOC_TYPEBITS :: CULong
pattern IOC_TYPEBITS = #{const _IOC_TYPEBITS}

pattern IOC_SIZEBITS :: CULong
pattern IOC_SIZEBITS = #{const _IOC_SIZEBITS}

pattern IOC_DIRBITS :: CULong
pattern IOC_DIRBITS = #{const _IOC_DIRBITS}

pattern IOC_NRMASK :: CULong
pattern IOC_NRMASK = #{const _IOC_NRMASK}

pattern IOC_TYPEMASK :: CULong
pattern IOC_TYPEMASK = #{const _IOC_TYPEMASK}

pattern IOC_SIZEMASK :: CULong
pattern IOC_SIZEMASK = #{const _IOC_SIZEMASK}

pattern IOC_DIRMASK :: CULong
pattern IOC_DIRMASK = #{const _IOC_DIRMASK}

pattern IOC_NRSHIFT :: CULong
pattern IOC_NRSHIFT = #{const _IOC_NRSHIFT}

pattern IOC_TYPESHIFT :: CULong
pattern IOC_TYPESHIFT = #{const _IOC_TYPESHIFT}

pattern IOC_SIZESHIFT :: CULong
pattern IOC_SIZESHIFT = #{const _IOC_SIZESHIFT}

pattern IOC_DIRSHIFT :: CULong
pattern IOC_DIRSHIFT = #{const _IOC_DIRSHIFT}

pattern IOC_NONE :: Word8
pattern IOC_NONE = #{const _IOC_NONE}

pattern IOC_WRITE :: Word8
pattern IOC_WRITE = #{const _IOC_WRITE}

pattern IOC_READ :: Word8
pattern IOC_READ = #{const _IOC_READ}

pattern IOC_WRITEREAD :: Word8
pattern IOC_WRITEREAD = #{const (_IOC_READ | _IOC_WRITE)}

pattern IOC :: Word8 -> Word8 -> Word8 -> Word16 -> CULong
pattern IOC di ty nr sz <- (splitIOC -> (di, ty, nr, sz))
  where IOC di ty nr sz =
          mkDirection di .|. mkType ty .|. mkNumber nr .|. mkSize sz

pattern IOR :: Word8 -> Word8 -> Word16 -> CULong
pattern IOR ty nr sz <- (splitIOC -> (IOC_READ, ty, nr, sz))
  where IOR ty nr sz = mkIOC IOC_READ ty nr sz

pattern IOW :: Word8 -> Word8 -> Word16 -> CULong
pattern IOW ty nr sz <- (splitIOC -> (IOC_WRITE, ty, nr, sz))
  where IOW ty nr sz = mkIOC IOC_WRITE ty nr sz

pattern IOWR :: Word8 -> Word8 -> Word16 -> CULong
pattern IOWR ty nr sz <- (splitIOC -> (IOC_WRITEREAD, ty, nr, sz))
  where IOWR ty nr sz = mkIOC IOC_WRITEREAD ty nr sz

mkIOC :: Word8 -> Word8 -> Word8 -> Word16 -> CULong
mkIOC di ty nr sz =
  mkDirection di .|. mkType ty .|. mkNumber nr .|. mkSize sz

splitIOC :: CULong -> (Word8, Word8, Word8, Word16)
splitIOC n = (getDirection n, getType n, getNumber n, getSize n)

mkDirection :: Word8 -> CULong
mkDirection n = fromIntegral n `shiftL` fromIntegral IOC_DIRSHIFT

getDirection :: CULong -> Word8
getDirection n =
  fromIntegral $ (n `shiftR` fromIntegral IOC_DIRSHIFT) .&. IOC_DIRMASK

mkType :: Word8 -> CULong
mkType n = fromIntegral n `shiftL` fromIntegral IOC_TYPESHIFT

getType :: CULong -> Word8
getType n =
  fromIntegral $ (n `shiftR` fromIntegral IOC_TYPESHIFT) .&. IOC_TYPEMASK

mkNumber :: Word8 -> CULong
mkNumber n = fromIntegral n `shiftL` fromIntegral IOC_NRSHIFT

getNumber :: CULong -> Word8
getNumber n =
  fromIntegral $ (n `shiftR` fromIntegral IOC_NRSHIFT) .&. IOC_NRMASK

mkSize :: Word16 -> CULong
mkSize n =
  fromIntegral n `shiftL` fromIntegral IOC_SIZESHIFT

getSize :: CULong -> Word16
getSize n =
  fromIntegral $ (n `shiftR` fromIntegral IOC_SIZESHIFT) .&. IOC_SIZEMASK

foreign import ccall "sys/ioctl.h"
  ioctl :: CInt -> CULong -> Ptr a -> IO CInt
