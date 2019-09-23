module Data.Multiformats.Multibase (
  Multibase(..), MultibaseError(..),
  encode, decode
) where

import Data.ByteString       as BS
import Data.ByteString.Char8 as Char8
import Data.Text             as T

-- Types

data Multibase =
  Identity | Base2
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
    Base2    -> coded '0'  base2Enc
  where
    coded char encoder = T.cons char . encoder

decode t | T.null t  = Left EmptyInput
         | otherwise =
  case T.head t of
    '\0' -> Right (Identity, idDec payload)
    _    -> Left UnknownCodec
  where
    payload = T.tail t

-- Helper Functions (Hidden)

idEnc :: ByteString -> Text
idEnc = T.pack . Char8.unpack
idDec :: Text -> ByteString
idDec = Char8.pack . T.unpack

base2Enc :: ByteString -> Text
base2Enc = idEnc
base2Dec :: Text -> ByteString
base2Dec = idDec
