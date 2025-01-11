module System.IO.GPIOD.Error where

import Data.Functor
import Control.Monad
import Control.Exception
import Data.Functor.Identity
import Data.Maybe
import Data.SOP
import Data.Text (Text)
import Data.Typeable
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Ptr
import qualified Data.Text as Text

newtype OneOf es = OneOf (NS Identity es)

instance All Show es => Show (OneOf es) where
  showsPrec n (OneOf ns) =
    hcollapse $ hcmap (Proxy :: Proxy Show) (K . showsPrec n . runIdentity) ns

deriving instance All (Compose Eq Identity) es => Eq (OneOf es)

data GPIODError es
  = KnownError (OneOf es)
  | UnknownError IOError

deriving instance All Show es => Show (GPIODError es)
deriving instance All (Compose Eq Identity) es => Eq (GPIODError es)

instance (Show (GPIODError es), Typeable es) => Exception (GPIODError es) where

type AnyGPIODError = GPIODError '[]

shiftGPIODError :: GPIODError es -> GPIODError (e ': es)
shiftGPIODError = \case
  KnownError (OneOf ns) -> KnownError $ OneOf $ S ns
  UnknownError ioe -> UnknownError ioe

unshiftGPIODError :: GPIODError (e ': es) -> Either (GPIODError es) e
unshiftGPIODError = \case
  KnownError (OneOf (S rest)) -> Left (KnownError (OneOf rest))
  KnownError (OneOf (Z (Identity e))) -> Right e
  UnknownError ioe -> Left (UnknownError ioe)

class IntoError e es where
  gpiodError :: e -> GPIODError es

instance {-# OVERLAPPING #-} IntoError e (e ': es) where
  gpiodError e = KnownError $ OneOf $ Z $ Identity e

instance {-# OVERLAPPABLE #-} IntoError e xs => IntoError e (x ': xs) where
  gpiodError e = shiftGPIODError (gpiodError e)

class AllIntoError from into where
  allIntoError :: GPIODError from -> GPIODError into

instance AllIntoError '[] into where
  allIntoError (UnknownError ioe) = UnknownError ioe

instance (IntoError f into, AllIntoError fs into) => AllIntoError (f ': fs) into where
  allIntoError = \case
    KnownError (OneOf (Z (Identity e))) -> gpiodError e
    KnownError (OneOf (S rest)) -> allIntoError $ KnownError $ OneOf rest
    UnknownError ioe -> UnknownError ioe

class LookupErrno e where
  fromErrnoMaybe :: Errno -> Maybe e

instance LookupErrno e => LookupErrno (Identity e) where
  fromErrnoMaybe e = Identity <$> fromErrnoMaybe e

type AllFromErrno es = All LookupErrno es

data ChipDoesNotExist = ChipDoesNotExist
  deriving (Show, Eq, Ord)

instance LookupErrno ChipDoesNotExist where
  fromErrnoMaybe e = guard (e == eNOENT) $> ChipDoesNotExist

data LineDoesNotExist = LineDoesNotExist
  deriving (Show, Eq, Ord)

instance LookupErrno LineDoesNotExist where
  fromErrnoMaybe e = guard (e == eNOENT || e == eINVAL) $> LineDoesNotExist

data LineOperationNotPermitted = LineOperationNotPermitted
  deriving (Show, Eq, Ord)

instance LookupErrno LineOperationNotPermitted where
  fromErrnoMaybe e = guard (e == ePERM) $> LineOperationNotPermitted

data LineAlreadyReserved = LineAlreadyReserved
  deriving (Show, Eq, Ord)

instance LookupErrno LineAlreadyReserved where
  fromErrnoMaybe e = guard (e == eBUSY) $> LineAlreadyReserved

mkFromErrno :: AllFromErrno es => Text -> Errno -> GPIODError es
mkFromErrno l e =
  case firstJust $ hcpure (Proxy :: Proxy LookupErrno) (fromErrnoMaybe e) of
    Nothing -> UnknownError (errnoToIOError (Text.unpack l) e Nothing Nothing)
    Just r -> KnownError (OneOf r)

fromPtr :: AllFromErrno es => Text -> Ptr a -> IO b -> IO (Either (GPIODError es) b)
fromPtr l ptr f =
  if ptr == nullPtr
    then do
      errno <- getErrno
      pure $ Left $ mkFromErrno l errno
    else Right <$> f

firstJust :: SListI xs => NP Maybe xs -> Maybe (NS Identity xs)
firstJust xs =
  case catMaybes $ map (htraverse' (fmap Identity)) $ hapInjs xs of
    [] -> Nothing
    x : _ -> Just x

fromPtrIO :: AllFromErrno es => Text -> IO (Ptr a) -> (Ptr a -> IO b) -> IO (Either (GPIODError es) b)
fromPtrIO l fptr f = do
  ptr <- fptr
  fromPtr l ptr (f ptr)

fromCInt :: AllFromErrno es => Text -> CInt -> IO a -> IO (Either (GPIODError es) a)
fromCInt l n act = case n of
  -1 -> do
    errno <- getErrno
    pure $ Left $ mkFromErrno l errno
  _other -> Right <$> act

fromCIntIO :: AllFromErrno es => Text -> IO CInt -> (CInt -> IO a) -> IO (Either (GPIODError es) a)
fromCIntIO l n act = do
  r <- n
  fromCInt l r (act r)
