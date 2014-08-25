{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Feldspar (Data(..),Length,WordN)
import Feldspar.Vector (mmMult, Pull(..), DIM2)
import Feldspar.Compiler
import Feldspar.Compiler.Plugin (loadFunOpts,loadFunOptsWith,pack)
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

foreign import ccall unsafe "MatMulC.h MatMulCopt" matMulCopt :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

matmul :: Pull DIM2 (Data Double) -> Pull DIM2 (Data Double) -> Pull DIM2 (Data Double)
matmul = mmMult True

loadFunOpts ["-optc=-O2"] 'matmul
loadFunOptsWith "_sics" sicsOptions ["-optc=-O2"] 'matmul

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

mkReferenceBench :: [Length] -> [Benchmark]
mkReferenceBench ls =
  [ env (setupRefEnv ls) $ \ ~(o,d) ->
    bench "C/matmul" (whnfIO $ matMulC (fromIntegral $ head ls) (fromIntegral $ product ls) d d o)
  , env (setupRefEnv ls) $ \ ~(o,d) ->
    bench "C/matmul_opt" (whnfIO $ matMulCopt (fromIntegral $ head ls) (fromIntegral $ product ls) d d o)
  ]

mkCompiledBench :: [Length] -> [Benchmark]
mkCompiledBench ls =
  [ env (setupCompEnv ls) $ \ ~(o,d) ->
    bench "Feldspar_C/matmul" (whnfIO $ c_matmul_raw d d o)
    -- _sicsmatmul_raw is a terrible name, but we can only configure the
    -- function prefix in multistage plugins. Luckily this does not leak to
    -- the outside world.
  , env (setupCompEnv ls) $ \ ~(o,d) ->
    bench "Feldspar_C/sics_matmul" (whnfIO $ _sicsmatmul_raw d d o)
  ]

-- | Create a benchmark that compares references and Feldspar for a specific
--   input.
mkComparison :: [Length] -> Benchmark
mkComparison ls = bgroup (dimToString ls) $
                    mkReferenceBench ls ++ mkCompiledBench ls

main :: IO ()
main = do
    setupPlugins
    defaultMainWith (mkConfig "report_matmul.html") $ map mkComparison sizes
