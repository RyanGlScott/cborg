{-# LANGUAGE RankNTypes #-}
module Tests.Embedded (
    testTree
  ) where

import           Control.Exception (throw)

import qualified Data.ByteString.Lazy as BSL

import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import           Codec.CBOR.FlatTerm
import           Codec.CBOR.Read
import           Codec.CBOR.Term
import           Codec.CBOR.Write

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Tests.Term (eqTerm)

{-
-- | TODO RGS: Docs
data TermWithSize = TermWithSize !Word Term

instance Arbitrary TermWithSize where
  arbitrary = (\t -> TermWithSize (fromIntegral (BSL.length (toLazyByteString (encodeTerm t)))) t) <$> arbitrary

-- | TODO RGS: Docs
encodeEmbeddedCBOR' :: TermWithSize -> Encoding
encodeEmbeddedCBOR' (TermWithSize sz t) = encodeEmbeddedCBOR sz $ encodeTerm t

-- | TODO RGS: Docs
decodeEmbeddedCBOR' :: Decoder s TermWithSize
decodeEmbeddedCBOR' = decodeEmbeddedCBOR decodeTerm
-}

-- | A typical roundtripping through CBOR.
roundtripNormal :: Term -> Term
roundtripNormal t
  = deserialise decodeTerm
  $ toLazyByteString
  $ encodeTerm t

-- | A roundtripping that also serialises the 'BSL.ByteString' representing the
-- serialised value.
roundtripViaSerialisedCBOR :: Term -> Term
roundtripViaSerialisedCBOR t
  = deserialise decodeTerm
  $ BSL.fromStrict
  $ deserialise decodeBytes
  $ toLazyByteString
  $ encodeBytes
  $ toStrictByteString
  $ encodeTerm t

-- | A roundtripping that embeds the CBOR of the serialised value.
roundtripViaEmbeddedCBOR :: Term -> Term
roundtripViaEmbeddedCBOR t
  = deserialise (decodeEmbeddedCBOR decodeTerm)
  $ toLazyByteString
  $ encodeEmbeddedCBOR sz
  $ encodeTerm t
  where
    sz = fromIntegral $ BSL.length $ toLazyByteString $ encodeTerm t

-- | TODO RGS: Docs
roundtripFlatTermNormal :: Term -> Term
roundtripFlatTermNormal t
  = fromFlatTerm' decodeTerm
  $ toFlatTerm
  $ encodeTerm t

-- | TODO RGS: Docs
roundtripFlatTermViaEmbeddedCBOR :: Term -> Term
roundtripFlatTermViaEmbeddedCBOR t
  = fromFlatTerm' (decodeEmbeddedCBOR decodeTerm)
  $ toFlatTerm
  $ encodeEmbeddedCBOR sz
  $ encodeTerm t
  where
    sz = fromIntegral $ BSL.length $ toLazyByteString $ encodeTerm t

-- | A typical roundtrip should produce the same result as a roundtripping
-- via embedded CBOR.
prop_roundtripTypicalEmbedded :: Term -> Bool
prop_roundtripTypicalEmbedded t =
  roundtripNormal t `eqTerm` roundtripViaEmbeddedCBOR t

-- | A roundtrip that serialises the intermediate 'BSL.ByteString' should
-- produce the same result as a roundtripped via embedded CBOR.
prop_roundtripSerialisedEmbedded :: Term -> Bool
prop_roundtripSerialisedEmbedded t =
  roundtripViaSerialisedCBOR t `eqTerm` roundtripViaEmbeddedCBOR t

-- | TODO RGS: Docs
prop_roundtripFlatTerm :: Term -> Bool
prop_roundtripFlatTerm t =
  roundtripFlatTermNormal t `eqTerm` roundtripFlatTermViaEmbeddedCBOR t

deserialise :: (forall s. Decoder s a) -> BSL.ByteString -> a
deserialise dec bytes =
  case deserialiseFromBytes dec bytes of
    Left failure        -> throw failure
    Right (trailing, _)  | not (BSL.null trailing)
                        -> error "Tests.Embedded.deserialise: trailing data"
    Right (_, t)        -> t

fromFlatTerm' :: (forall s. Decoder s a) -> FlatTerm -> a
fromFlatTerm' dec ft =
  case fromFlatTerm dec ft of
    Left msg -> error msg
    Right t  -> t

--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree =
  testGroup "embedded CBOR-in-CBOR"
  [ testProperty "equivalent to typical roundtrip" prop_roundtripTypicalEmbedded
  , testProperty "equivalent to (de-)serialising CBOR roundtrip" prop_roundtripSerialisedEmbedded
  , testProperty "equivalent to typical FlatTerm roundtrip" prop_roundtripFlatTerm
  ]
