{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Feldspar (Data(..),Length,WordN)
import Feldspar.Vector (mmMult, Pull(..), DIM2)
import Feldspar.Compiler
import Feldspar.Compiler.Plugin (loadFunOpts,pack)
import Feldspar.Compiler.Marshal (SA)
import GHC.Ptr

import Control.Monad (forM)
import Foreign.Marshal (with,withArray,allocaArray)
-- Terrible error messages if constructors are not imported. See
-- https://ghc.haskell.org/trac/ghc/ticket/5610 for more information.
import Foreign.C.Types (CInt(..), CDouble(..))
import Foreign.Ptr (Ptr(..))
import Data.Default
import Control.DeepSeq (force)
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

main :: IO ()
main = with def $ \out -> do
  allocaArray (512*512) $ \out' -> do
    td <- evaluate $ take (512*512) (map realToFrac testdata :: [CDouble])
    withArray td $ \d' -> do
      let lss = map (map (*len)) [[1,1],[2,2],[4,4],[8,8]]
      bs <- forM lss $ \ls -> do
              d <- mkData testdata ls
              mkBench "c_matmul" ls (c_matmul_raw d d out)
      rs <- forM lss $ \ls -> do
              mkBench "matMulC" ls (matMulC (fromIntegral $ head ls) (fromIntegral $ product ls) d' d' out')
      _ <- evaluate c_matmul_builder
      defaultMainWith (mkConfig "report_matmul.html") (return ())
        [ bgroup "reference" rs
        , bgroup "compiled" bs
        ]
