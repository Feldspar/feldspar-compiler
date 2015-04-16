{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Test that the compiler respects the calling conventions of Feldspar

module Main where

import qualified Prelude

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Feldspar hiding (assert)
import Feldspar.Vector
import Feldspar.Compiler
import Feldspar.Compiler.Plugin

import Control.Applicative

vector1D :: Length -> Gen a -> Gen [a]
vector1D l = vectorOf (Prelude.fromIntegral l)

pairArg :: (Data Word8,Data IntN) -> Data IntN
pairArg (a,b) = i2n a + b

pairRes :: Data Word16 -> (Data WordN, Data IntN)
pairRes a = (i2n a, i2n a)

vecId :: Pull1 Word32 -> Pull1 Word32
vecId = id

vectorInPair :: (Pull1 WordN, Data WordN) -> (Data WordN, Pull1 WordN)
vectorInPair (v,a) = (a,v)

vectorInVector :: Pull DIM1 (Pull1 WordN) -> Data WordN
vectorInVector v = fromZero $ sum $ map (fromZero . sum) v

vectorInPairInVector :: Data WordN -> Pull DIM1 (Data WordN, Pull1 WordN)
vectorInPairInVector l = indexed1 l $ \i -> (i, indexed1 i id)

loadFun 'pairArg
loadFun 'pairRes
loadFun 'vecId
loadFun 'vectorInPair
loadFun 'vectorInVector
loadFun 'vectorInPairInVector

prop_pairArg = eval pairArg ==== c_pairArg
prop_pairRes = eval pairRes ==== c_pairRes
prop_vecId (Small l) =
    forAll (vector1D l arbitrary) $ \xs ->
      eval vecId xs ==== c_vecId xs
prop_vectorInPair (Small l) =
    forAll ((,) <$> vector1D l arbitrary <*> arbitrary) $ \p ->
      eval vectorInPair p ==== c_vectorInPair p
prop_vectorInVector (Small l1) (Small l2) =
    forAll (vector1D l1 (vector1D l2 arbitrary)) $ \v ->
      eval vectorInVector v ==== c_vectorInVector v
prop_vectorInPairInVector (Small l) = eval vectorInPairInVector l ==== c_vectorInPairInVector l

tests :: TestTree
tests = testGroup "CallingConvention"
    [ testProperty "pairArg" prop_pairArg
    , testProperty "pairRes" prop_pairRes
    , testProperty "vecId"   prop_vecId
    , testProperty "vectorInPair" prop_vectorInPair
    , testProperty "vectorInVector" prop_vectorInVector
    -- TODO: This test case will cause a segmentation fault due to issue #145
    -- , testProperty "vectorInPairInVector" prop_vectorInPairInVector
    ]

main :: IO ()
main = defaultMain tests
