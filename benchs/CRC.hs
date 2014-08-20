{-# LANGUAGE TemplateHaskell #-}

import Feldspar hiding (force)
import Feldspar.Vector
import Feldspar.Compiler.Plugin
import Feldspar.Algorithm.CRC

import Foreign.Marshal (with)
import Data.Default
import Control.DeepSeq (force)
import Control.Exception (evaluate)

import BenchmarkUtils
import Criterion.Main

len :: Length
len = 16 * 1024

testdata :: [Word8]
testdata = Prelude.take (fromIntegral len) $ cycle [1,2,3,4]

naive :: Pull1 Word8 -> Data Word16
naive = crcNaive 0x8005 0

normal :: Pull1 Word8 -> Data Word16
normal v = share (makeCrcTable 0x8005) $ \t -> crcNormal t 0 v

h_naive :: ([Length],[Word8]) -> Word16
h_naive = eval naive
loadFun 'naive

h_normal :: ([Length],[Word8]) -> Word16
h_normal = eval normal
loadFun 'normal

main :: IO ()
main = with def $ \out -> do
    d  <- evaluate $ force testdata
    pd <- pack ([len],d) >>= evaluate
    _  <- evaluate c_naive_builder
    _  <- evaluate c_normal_builder
    defaultMainWith (mkConfig "report_crc.html")
      [
        bgroup "evaluated"
          [ bench "h_naive"  $ nf h_naive  ([1024],Prelude.take 1024 d)
          , bench "h_normal" $ nf h_normal ([1024],Prelude.take 1024 d)
          ]
      , bgroup "compiled"
          [ bgroup "marshal"
              [ bench "c_naive"  $ whnfIO $ c_naive_worker ([len],d)
              , bench "c_normal" $ whnfIO $ c_normal_worker ([len],d)
              ]
          , bgroup "raw"
              [ bench "c_naive"  $ whnfIO $ c_naive_raw pd out
              , bench "c_normal" $ whnfIO $ c_normal_raw pd out
              ]
          ]
      ]

