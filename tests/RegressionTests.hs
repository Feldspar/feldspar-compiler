module Main where

-- To generate the golden files use a script similiar to this one
-- > ghc -ilib -isrc -itests -e 'compile example9 "tests/gold/example9" "example9" defaultOptions' tests/RegressionTests.hs

import Test.Framework
import Test.Golden

import qualified Prelude
import Feldspar
import Feldspar.Compiler

example9 :: Data Int32 -> Data Int32
example9 a = condition (a<5) (3*(a+20)) (30*(a+20))

topLevelConsts :: Data Index -> Data Index -> Data Index
topLevelConsts a b = condition (a<5) (d ! (b+5)) (c ! (b+5))
  where
    c = value [1,2,3,4,5] :: Data [Index]
    d = value [2,3,4,5,6] :: Data [Index]

tests = testGroup "RegressionTests" 
    [ goldenVsFile "example9" "tests/gold/example9.c" "tests/example9.c" $ compile example9 "tests/example9" "example9" defaultOptions
    , goldenVsFile "topLevelConsts" "tests/gold/topLevelConsts.c" "tests/topLevelConsts.c" $ compile topLevelConsts "tests/topLevelConsts" "topLevelConsts" defaultOptions
    ]

main = defaultMain [tests]

