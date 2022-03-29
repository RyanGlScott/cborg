{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Embedded.Roundtrip.Normal
  ( roundtripNormal
  ) where

import           Control.Exception (throw)
import qualified Data.ByteString.Lazy as BSL

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Codec.Serialise (Serialise(..))

import           Micro.CBOR ()
import           Micro.Types (Tree)

-- | A typical roundtripping through CBOR.
roundtripNormal :: Tree -> Tree
roundtripNormal t
  = deserialiseWith (decode @Tree)
  $ toLazyByteString
  $ encode @Tree t

deserialiseWith :: (forall s. Decoder s a) -> BSL.ByteString -> a
deserialiseWith dec bytes =
  case deserialiseFromBytes dec bytes of
    Left failure        -> throw failure
    Right (trailing, _)  | not (BSL.null trailing)
                        -> error "Embedded.Roundtrip.Normal.deserialiseWith: trailing data"
    Right (_, t)        -> t
