{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

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

pairArg :: (Data Word8,Data IntN) -> Data IntN
pairArg (a,b) = i2n a + b

pairRes :: Data Word16 -> (Data WordN, Data IntN)
pairRes a = (i2n a, i2n a)

vecId :: Vector1 Word32 -> Vector1 Word32
vecId = id

vectorInPair :: (Vector1 WordN, Data WordN) -> (Data WordN, Vector1 WordN)
vectorInPair (v,a) = (a,v)

vectorInVector :: Vector (Vector1 WordN) -> Data WordN
vectorInVector v = sum $ map sum v

vectorInPairInVector :: Data WordN -> Vector (Data WordN, Vector1 WordN)
vectorInPairInVector l = indexed l $ \i -> (i, indexed i id)

loadFun 'pairArg
loadFun 'pairRes
loadFun 'vecId
loadFun 'vectorInPair
loadFun 'vectorInVector
loadFun 'vectorInPairInVector

prop_pairArg      = eval pairArg === c_pairArg
prop_pairRes      = eval pairRes === c_pairRes
prop_vecId        = eval vecId   === c_vecId
prop_vectorInPair = eval vectorInPair === c_vectorInPair
prop_vectorInVector = eval vectorInVector === c_vectorInVector
prop_vectorInPairInVector = forAll (choose (0,63)) $ \l -> eval vectorInPairInVector l === c_vectorInPairInVector l

tests :: TestTree
tests = testGroup "CodeGeneration"
    [ testProperty "pairArg" prop_pairArg
    , testProperty "pairRes" prop_pairRes
    , testProperty "vecId"   prop_vecId
    , testProperty "vectorInPair" prop_vectorInPair
    , testProperty "vectorInVector" prop_vectorInVector
    , testProperty "vectorInPairInVector" prop_vectorInPairInVector
    ]

main :: IO ()
main = defaultMain tests
