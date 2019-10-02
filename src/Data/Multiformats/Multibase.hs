module Data.Multiformats.Multibase (
  Multibase(..), MultibaseError(..),
  encode, decode
) where

import qualified Data.BaseN  as BaseN
import Data.ByteString       as BS
import Data.ByteString.Char8 as Char8
import Data.Text             as T

-- Types

data Multibase =
  Identity | Base2 | Base8
  deriving (Eq, Show)
data MultibaseError =
  EmptyInput | UnknownCodec | CodecError
  deriving (Eq, Show)

 -- Signatures

encode :: Multibase -> ByteString -> Text
decode :: Text -> Either MultibaseError (Multibase, ByteString)

-- Implementations

encode codec =
  case codec of
    Identity -> coded '\0' idEnc
    Base2    -> coded '0'  BaseN.encodeBase2
    Base8    -> coded '7'  BaseN.encodeBase8
  where
    coded char encoder = T.cons char . encoder

decode t | T.null t  = Left EmptyInput
         | otherwise =
  case T.head t of
    '\0' -> Right (Identity, idDec payload)
    '0'  -> case BaseN.decodeBase2 payload of
      (Left _)  -> Left CodecError
      (Right r) -> Right (Base2, r)
    '7'  -> case BaseN.decodeBase8 payload of
      (Left _)  -> Left CodecError
      (Right r) -> Right (Base8, r)
    _    -> Left UnknownCodec
  where
    payload = T.tail t

-- Helper Functions (Hidden)

idEnc :: ByteString -> Text
idEnc = T.pack . Char8.unpack
idDec :: Text -> ByteString
idDec = Char8.pack . T.unpack
