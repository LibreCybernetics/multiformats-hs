module Data.Multiformats.Multibase (
  Multibase(Identity), MultibaseError,
  encode, decode
) where

import Data.ByteString       as BS
import Data.ByteString.Char8 as Char8
import Data.Text             as T

-- Types

data Multibase = Identity
data MultibaseError = Empty | UnkownCodec | CodecError

 -- Signatures

encode :: Multibase -> ByteString -> Text
decode :: Text -> Either MultibaseError (Multibase, ByteString)

-- Implementations

encode Identity bs =  '\0' `T.cons` idEnc bs

decode t | T.null t  = Left Empty
         | otherwise =
 case T.head t of
   '\0' -> Right (Identity, idDec payload)
   _    -> Left UnkownCodec
   where payload = T.tail t

-- Helper Functions (Hidden)

idEnc :: ByteString -> Text
idEnc = T.pack . Char8.unpack
idDec :: Text -> ByteString
idDec = Char8.pack . T.unpack
