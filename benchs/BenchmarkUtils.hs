module BenchmarkUtils where

import Feldspar.Compiler.Plugin (pack)
import Control.Exception (evaluate)
import Criterion.Main
import Criterion.Types
import Data.List (intercalate)

mkConfig report = defaultConfig { forceGC    = True
                                , reportFile = Just report
                                }

dimToString ls = intercalate "x" (map show ls)

mkData ds ls = do putStrLn $ unwords ["Alloc array with", dimToString ls, "elements"]
                  evaluate =<< pack (ls, take (fromIntegral $ product ls) ds)

mkBench name ls fun = bench (name ++ "_" ++ dimToString ls) fun
