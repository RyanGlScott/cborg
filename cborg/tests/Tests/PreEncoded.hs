module Tests.PreEncoded (
    testTree
  ) where

import           Data.Monoid (Monoid(mconcat))

import           Codec.CBOR.Term     (Term, encodeTerm)
import           Codec.CBOR.FlatTerm (FlatTerm, toFlatTerm, TermToken(..))
import           Codec.CBOR.Write    (toStrictByteString, toLazyByteString)
import           Codec.CBOR.Encoding (Encoding, encodePreEncoded, encodePreEncodedLazy)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Tests.Term () -- instance Arbitrary Term
import           Tests.Reference.Generators
                   (canonicalNaN, floatToWord, doubleToWord)


-- | Use 'encodePreEncoded' but with a serialised term as the bytes.
--
encodePreEncoded' :: Term -> Encoding
encodePreEncoded' = encodePreEncoded . toStrictByteString . encodeTerm

-- | Like `encodePreEncoded'`, but using a lazy ByteString as the intermediate
-- format.
encodePreEncodedLazy' :: Term -> Encoding
encodePreEncodedLazy' = encodePreEncodedLazy . toLazyByteString . encodeTerm


prop_preEncodedTerm_sameBytes :: Term -> Bool
prop_preEncodedTerm_sameBytes = prop_preEncodedTerm_sameBytes' encodePreEncoded'

prop_preEncodedTerm_sameTokens :: Term -> Bool
prop_preEncodedTerm_sameTokens = prop_preEncodedTerm_sameTokens' encodePreEncoded'

prop_preEncodedTerms_sameBytes :: [(Term, Bool)] -> Bool
prop_preEncodedTerms_sameBytes = prop_preEncodedTerms_sameBytes' encodePreEncoded'

prop_preEncodedTerms_sameTokens :: [(Term, Bool)] -> Bool
prop_preEncodedTerms_sameTokens = prop_preEncodedTerms_sameTokens' encodePreEncoded'


prop_preEncodedTermLazy_sameBytes :: Term -> Bool
prop_preEncodedTermLazy_sameBytes = prop_preEncodedTerm_sameBytes' encodePreEncodedLazy'

prop_preEncodedTermLazy_sameTokens :: Term -> Bool
prop_preEncodedTermLazy_sameTokens = prop_preEncodedTerm_sameTokens' encodePreEncodedLazy'

prop_preEncodedTermsLazy_sameBytes :: [(Term, Bool)] -> Bool
prop_preEncodedTermsLazy_sameBytes = prop_preEncodedTerms_sameBytes' encodePreEncodedLazy'

prop_preEncodedTermsLazy_sameTokens :: [(Term, Bool)] -> Bool
prop_preEncodedTermsLazy_sameTokens = prop_preEncodedTerms_sameTokens' encodePreEncodedLazy'


prop_preEncodedTerm_sameBytes' :: (Term -> Encoding) -> Term -> Bool
prop_preEncodedTerm_sameBytes' encodePre t =
    sameBytes
      (encodeTerm t)
      (encodePre t)


prop_preEncodedTerm_sameTokens' :: (Term -> Encoding) -> Term -> Bool
prop_preEncodedTerm_sameTokens' encodePre t =
    sameTokens
      (encodeTerm t)
      (encodePre t)


prop_preEncodedTerms_sameBytes' :: (Term -> Encoding) -> [(Term, Bool)] -> Bool
prop_preEncodedTerms_sameBytes' encodePre ts  =
    sameBytes
      (mconcat [ encodeTerm t | (t, _) <- ts ])
      (mconcat [ if pre then encodePre t
                        else encodeTerm t
               | (t, pre) <- ts ])

prop_preEncodedTerms_sameTokens' :: (Term -> Encoding) -> [(Term, Bool)] -> Bool
prop_preEncodedTerms_sameTokens' encodePre ts  =
    sameTokens
      (mconcat [ encodeTerm t | (t, _) <- ts ])
      (mconcat [ if pre then encodePre t
                        else encodeTerm t
               | (t, pre) <- ts ])


sameBytes :: Encoding -> Encoding -> Bool
sameBytes e1 e2 = toLazyByteString e1 == toLazyByteString e2

sameTokens :: Encoding -> Encoding -> Bool
sameTokens e1 e2 = canonicaliseFlatTerm (toFlatTerm e1)
      `eqFlatTerm` canonicaliseFlatTerm (toFlatTerm e2)

canonicaliseFlatTerm :: FlatTerm -> FlatTerm
canonicaliseFlatTerm = map canonicaliseTermToken

canonicaliseTermToken :: TermToken -> TermToken
canonicaliseTermToken (TkFloat16 f) | isNaN f = TkFloat16 canonicalNaN
canonicaliseTermToken (TkFloat32 f) | isNaN f = TkFloat16 canonicalNaN
canonicaliseTermToken (TkFloat64 f) | isNaN f = TkFloat16 canonicalNaN
canonicaliseTermToken x = x

eqFlatTerm :: FlatTerm -> FlatTerm -> Bool
eqFlatTerm x y = and (zipWith eqTermToken x y)

-- NaNs strike again!
eqTermToken :: TermToken -> TermToken -> Bool
eqTermToken (TkFloat16 x) (TkFloat16 y) = floatToWord  x == floatToWord  y
eqTermToken (TkFloat32 x) (TkFloat32 y) = floatToWord  x == floatToWord  y
eqTermToken (TkFloat64 x) (TkFloat64 y) = doubleToWord x == doubleToWord y
eqTermToken x y = x == y


--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree =
  testGroup "pre-encoded"
  [ testGroup "strict ByteStrings"
    [ testProperty "single term, same bytes"   prop_preEncodedTerm_sameBytes
    , testProperty "single term, same tokens"  prop_preEncodedTerm_sameTokens
    , testProperty "list terms, same bytes"    prop_preEncodedTerms_sameBytes
    , testProperty "list terms, same tokens"   prop_preEncodedTerms_sameTokens
    ]
  , testGroup "lazy ByteStrings"
    [ testProperty "single term, same bytes"   prop_preEncodedTermLazy_sameBytes
    , testProperty "single term, same tokens"  prop_preEncodedTermLazy_sameTokens
    , testProperty "list terms, same bytes"    prop_preEncodedTermsLazy_sameBytes
    , testProperty "list terms, same tokens"   prop_preEncodedTermsLazy_sameTokens
    ]
  ]

