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

import Control.Applicative

withVector1D :: Int -> Gen a -> Gen ([WordN],[a])
withVector1D l ga = do
    xs <- vectorOf l ga
    return ([Prelude.fromIntegral l],xs)

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

prop_pairArg      = eval pairArg === c_pairArg
prop_pairRes      = eval pairRes === c_pairRes
prop_vecId        = sized $ \l -> do
                       xs <- withVector1D l arbitrary
                       eval vecId xs === c_vecId xs
prop_vectorInPair = sized $ \l -> do
                      p <- (,) <$> withVector1D l arbitrary <*> arbitrary
                      eval vectorInPair p === c_vectorInPair p
prop_vectorInVector = sized $ \l1 -> do
                        sized $ \l2 -> do
                          v <- withVector1D l1 (withVector1D l2 arbitrary)
                          eval vectorInVector v === c_vectorInVector v
prop_vectorInPairInVector = forAll (choose (0,63)) $ \l ->
                              eval vectorInPairInVector l === c_vectorInPairInVector l

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
