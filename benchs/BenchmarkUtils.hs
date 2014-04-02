module BenchmarkUtils where

import Feldspar.Compiler.Plugin (pack)
import Control.Exception (evaluate)
import Criterion.Main (bench)
import Criterion.Config (Config(..),defaultConfig,ljust)
import Data.List (intersperse)

mkConfig report = defaultConfig { cfgPerformGC = ljust True
                                , cfgReport    = ljust report
                                }

dimToString ls = concat $ intersperse "x" (map show ls)

mkData ds ls = do putStrLn $ unwords ["Alloc array with", dimToString ls, "elements"]
                  evaluate =<< pack (ls, take (fromIntegral $ product ls) ds)

mkBench name ls fun = do
    return $ bench (name ++ "_" ++ dimToString ls) fun
