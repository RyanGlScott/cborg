{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Embedded.Roundtrip.EmbeddedCBOR
  ( roundtripViaEmbeddedCBOR
  ) where

import           Control.Exception (throw)
import qualified Data.ByteString.Lazy as BSL

import           Codec.CBOR.Decoding (Decoder, decodeEmbeddedCBOR)
import           Codec.CBOR.Encoding (encodeEmbeddedCBOR)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Codec.Serialise (Serialise(..))

import           Micro.CBOR ()
import           Micro.Types (Tree)

-- | A roundtripping that embeds the CBOR of the serialised value.
roundtripViaEmbeddedCBOR :: Word -> Tree -> Tree
roundtripViaEmbeddedCBOR sz t
  = deserialiseWith (decodeEmbeddedCBOR (decode @Tree))
  $ toLazyByteString
  $ encodeEmbeddedCBOR sz
  $ encode @Tree t

deserialiseWith :: (forall s. Decoder s a) -> BSL.ByteString -> a
deserialiseWith dec bytes =
  case deserialiseFromBytes dec bytes of
    Left failure        -> throw failure
    Right (trailing, _)  | not (BSL.null trailing)
                        -> error "Embedded.Roundtrip.EmbeddedCBOR.deserialiseWith: trailing data"
    Right (_, t)        -> t
