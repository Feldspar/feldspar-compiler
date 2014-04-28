{-# LANGUAGE TemplateHaskell #-}

import Feldspar hiding (force)
import Feldspar.Vector
import Feldspar.Compiler.Plugin
import Feldspar.Algorithm.CRC

import Foreign.Marshal (with)
import Data.Default
import Control.DeepSeq (force)
import Control.Exception (evaluate)

import Criterion.Main

testdata :: [Word8]
testdata = Prelude.take (16*1024) $ cycle [1,2,3,4]

naive :: Vector1 Word8 -> Data Word16
naive = crcNaive 0x8005 0

normal :: Vector1 Word8 -> Data Word16
normal v = share (makeCrcTable 0x8005) $ \t -> crcNormal t 0 v

h_naive :: [Word8] -> Word16
h_naive = eval naive

h_normal :: [Word8] -> Word16
h_normal = eval normal

loadFun ['naive, 'normal]

main :: IO ()
main = with def $ \out -> do
    d  <- evaluate $ force testdata
    pd <- pack d >>= evaluate
    _  <- evaluate c_naive_builder
    _  <- evaluate c_normal_builder
    defaultMain
      [
        bgroup "evaluated"
          [ bench "h_naive"  $ nf h_naive  (Prelude.take 1024 d)
          , bench "h_normal" $ nf h_normal (Prelude.take 1024 d)
          ]
      , bgroup "compiled"
          [ bgroup "marshal"
              [ bench "c_naive"  $ c_naive_worker d
              , bench "c_normal" $ c_normal_worker d
              ]
          , bgroup "raw"
              [ bench "c_naive"  $ c_naive_raw pd out
              , bench "c_normal" $ c_normal_raw pd out
              ]
          ]
      ]

