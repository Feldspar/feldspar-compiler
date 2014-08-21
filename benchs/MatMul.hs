{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Feldspar (Data(..),Length,WordN)
import Feldspar.Vector (mmMult, Pull(..), DIM2)
import Feldspar.Compiler
import Feldspar.Compiler.Plugin (loadFunOpts,pack)
import Feldspar.Compiler.Marshal (Marshal(..),SA(..),allocSA)

import Foreign.Marshal (new,newArray,mallocArray)
-- Terrible error messages if constructors are not imported. See
-- https://ghc.haskell.org/trac/ghc/ticket/5610 for more information.
import Foreign.C.Types (CInt(..), CDouble(..))
import Foreign.Ptr (Ptr(..))
import Control.DeepSeq (NFData(..))
import Control.Exception (evaluate)

import BenchmarkUtils
import Criterion.Main

testdata :: [Double]
testdata = cycle [1.1,2.2,3.3,4.4]

foreign import ccall unsafe "MatMulC.h MatMulC" matMulC :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

matmul :: Pull DIM2 (Data Double) -> Pull DIM2 (Data Double) -> Pull DIM2 (Data Double)
matmul = mmMult True

loadFunOpts ["-optc=-O2"] 'matmul

len :: Length
len = 64

sizes :: [[Length]]
sizes = map (map (*len)) [[1,1],[2,2],[4,4],[8,8]]

instance NFData (Ptr a) where

setupPlugins = do
    putStrLn "Compiling c_matmul plugin"
    evaluate c_matmul_builder

setupRefEnv :: [Length] -> IO (Ptr CDouble, Ptr CDouble)
setupRefEnv ls = do
    let len = fromIntegral $ product ls
    let td  = take len (map realToFrac testdata)
    o <- mallocArray len
    d <- newArray td
    return (o,d)

allocOut :: [Length] -> IO (Ptr (Ptr (SA Length),Ptr (SA Double)))
allocOut lengths = do
    ls <- pack lengths
    ds <- allocSA $ fromIntegral $ product lengths :: IO (Ptr (SA Double))
    new (ls,ds)

setupCompEnv ls = do
    o <- allocOut ls
    d <- mkData testdata ls
    return (o,d)

mkRef :: [Length] -> Benchmark
mkRef ls = env (setupRefEnv ls) $ \ ~(o,d) ->
    mkBench "matMulC" ls (whnfIO $ matMulC (fromIntegral $ head ls) (fromIntegral $ product ls) d d o)

mkComp :: [Length] -> Benchmark
mkComp ls = env (setupCompEnv ls) $ \ ~(o,d) ->
    mkBench "c_matmul" ls (whnfIO $ c_matmul_raw d d o)

main :: IO ()
main = defaultMainWith (mkConfig "report_matmul.html")
    [ bgroup "reference" $ map mkRef sizes
    , env setupPlugins $ \_ -> bgroup "compiled" $ map mkComp sizes
    ]
