{-# LANGUAGE TemplateHaskell #-}

module Main where

import Feldspar (Length,Complex)
import Feldspar.Algorithm.FFT
import Feldspar.Compiler
import Feldspar.Compiler.Plugin

import Control.Monad (forM)
import Foreign.Marshal (with)
import Data.Default
import Control.DeepSeq (force)
import Control.Exception (evaluate)

import BenchmarkUtils
import Criterion.Main

testdata :: [Complex Float]
testdata = cycle [1,2,3,4]

loadFunOpts ["-optc=-O2"] 'fft
loadFunOpts ["-optc=-O2"] 'ifft

len :: Length
len = 4096

main :: IO ()
main = with def $ \out -> do
    let lss = map (map (*len)) [[1],[2],[4],[8]]
    bs <- forM lss $ \ls -> do
            d <- mkData testdata ls
            mkBench "c_fft" ls (whnfIO $ c_fft_raw d out)
    _  <- evaluate c_fft_builder
    defaultMainWith (mkConfig "report_fft.html")
      [ bgroup "compiled" bs
      ]
