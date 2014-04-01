{-# LANGUAGE TemplateHaskell #-}

module Main where

import Feldspar (Data(..),Length,WordN)
import Feldspar.Vector (mmMult, Pull(..), DIM2)
import Feldspar.Compiler
import Feldspar.Compiler.Plugin (loadFunOpts,pack)
import Feldspar.Compiler.Marshal (SA)
import GHC.Ptr

import Control.Monad (forM)
import Foreign.Marshal (with)
import Data.Default
import Control.DeepSeq (force)
import Control.Exception (evaluate)

import BenchmarkUtils
import Criterion.Main

testdata :: [WordN]
testdata = cycle [1,2,3,4]

matmul :: Pull DIM2 (Data WordN) -> Pull DIM2 (Data WordN) -> Pull DIM2 (Data WordN)
matmul = mmMult

loadFunOpts ["-optc=-O2"] 'matmul

len :: Length
len = 64

main :: IO ()
main = with def $ \out -> do
    let lss = map (map (*len)) [[1,1],[2,2],[4,4],[8,8]]
    bs <- forM lss $ \ls -> do
            d <- mkData testdata ls
            mkBench "c_matmul" ls (c_matmul_raw d d out)
    _ <- evaluate c_matmul_builder
    defaultMainWith (mkConfig "report_matmul.html") (return ())
      [ bgroup "compiled" bs
      ]
