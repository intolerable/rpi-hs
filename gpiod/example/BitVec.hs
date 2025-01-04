module BitVec where

import Data.Proxy
import Data.Bits
import Data.Maybe
import Data.Vector.Unboxed ((!))
import Data.Word
import GHC.TypeLits
import Data.Bit (Bit(..), cloneToWords8, castFromWords)
import qualified Data.Vector.Unboxed as UV

newtype BitVec (n :: Nat) = BitVec (UV.Vector Bit)
  deriving (Show, Eq, Ord)

fromBool :: Bool -> BitVec 1
fromBool b = BitVec $ UV.singleton $ Bit b

fromWord :: forall n . KnownNat n => Word -> BitVec n
fromWord w = do
  let n = natVal (Proxy :: Proxy n)
  BitVec $ UV.take (fromIntegral n) $ castFromWords $ UV.singleton w

fromWord8 :: forall n . (KnownNat n, CmpNat n 9 ~ LT) => Word8 -> BitVec n
fromWord8 w = do
  let n = natVal (Proxy :: Proxy n)
  BitVec $ UV.take (fromIntegral n) $ castFromWords $ UV.singleton (fromIntegral w)

append :: BitVec a -> BitVec b -> BitVec (a + b)
append (BitVec a) (BitVec b) = BitVec (b <> a)

bitVecWord8 :: (KnownNat n, CmpNat n 9 ~ LT) => BitVec n -> Word8
bitVecWord8 (BitVec v) =
  fromJust $ toIntegralSized $ cloneToWords8 v ! 0
