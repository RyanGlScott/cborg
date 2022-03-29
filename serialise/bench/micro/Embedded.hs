module Embedded
  ( benchmarks -- :: [Benchmark]
  ) where

import           Criterion.Main
import qualified Data.ByteString.Lazy as BSL

import           Codec.CBOR.Write (toLazyByteString)
import           Codec.Serialise (Serialise(..))

import           Embedded.Roundtrip.EmbeddedCBOR (roundtripViaEmbeddedCBOR)
import           Embedded.Roundtrip.Normal (roundtripNormal)
import           Embedded.Roundtrip.SerialisedCBOR (roundtripViaSerialisedCBOR)
import qualified Micro.CBOR ()
import qualified Micro.DeepSeq ()
import qualified Micro.Load
import           Micro.Types (Tree)

benchmarks :: [Benchmark]
benchmarks =
  [ env (pure (tstdata, tstdataSz)) $ \ ~(t, tSz) ->
    bgroup "Roundtrips"
    [ bench "encode-decode" $
        nf roundtripNormal t
    , bench "encode-encodeBytes-decodeBytes-decode" $
        nf roundtripViaSerialisedCBOR t
    , bench "encode-encodeEmbeddedCBOR-decodeEmbeddedCBOR-decode" $
        nf (roundtripViaEmbeddedCBOR tSz) t
    ]
  ]
  where
    -- Input data
    tstdata :: Tree
    tstdata = Micro.Load.mkBigTree 16 -- tree of size 2^16

    tstdataSz :: Word
    tstdataSz = fromIntegral $ BSL.length $ toLazyByteString $ encode tstdata
