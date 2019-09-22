module Data.Multiformats.Multibase (
  Multibase(Identity), MultibaseError(Empty, UnknownCodec, CodecError),
  encode, decode
) where

import Data.ByteString       as BS
import Data.ByteString.Char8 as Char8
import Data.Text             as T

-- Types

data Multibase = Identity deriving (Eq, Show)
data MultibaseError = Empty | UnknownCodec | CodecError deriving (Eq, Show)

 -- Signatures

encode :: Multibase -> ByteString -> Text
decode :: Text -> Either MultibaseError (Multibase, ByteString)

-- Implementations

encode Identity bs =  '\0' `T.cons` idEnc bs

decode t | T.null t  = Left Empty
         | otherwise =
 case T.head t of
   '\0' -> Right (Identity, idDec payload)
   _    -> Left UnknownCodec
   where payload = T.tail t

-- Helper Functions (Hidden)

idEnc :: ByteString -> Text
idEnc = T.pack . Char8.unpack
idDec :: Text -> ByteString
idDec = Char8.pack . T.unpack
