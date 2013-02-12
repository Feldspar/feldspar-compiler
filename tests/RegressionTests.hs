module Main where

-- To generate the golden files use a script similiar to this one
-- > ghc -ilib -isrc -itests tests/RegressionTests.hs -e 'writeGoldFile example9 "example9" defaultOptions'
-- > ghc -ilib -isrc -itests tests/RegressionTests.hs -e 'writeGoldFile example9 "example9_native" nativeOpts'

import Test.Framework
import Test.Golden
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Prelude
import Feldspar
import Feldspar.Compiler
import Feldspar.Vector
import qualified Feldspar.Vector.Push as PV

import Data.Monoid ((<>))
import Shelly
import Data.Text.Lazy (pack)
import Filesystem.Path.CurrentOS (decodeString)

example9 :: Data Int32 -> Data Int32
example9 a = condition (a<5) (3*(a+20)) (30*(a+20))

topLevelConsts :: Data Index -> Data Index -> Data Index
topLevelConsts a b = condition (a<5) (d ! (b+5)) (c ! (b+5))
  where
    c = value [1,2,3,4,5] :: Data [Index]
    d = value [2,3,4,5,6] :: Data [Index]

pairParam :: (Data Index, Data Index) -> Data Index
pairParam (x, _) = x

pairParam2 :: (Data Int16, Data Int16) ->
              ((Data Int16, Data Int16), (Data Int16, Data Int16))
pairParam2 c = (c, c)

-- One test starting.
metrics :: Vector1 IntN -> Vector1 IntN
           -> Vector (Vector (Data Index, Data Index)) -> Vector (Vector1 IntN)
metrics s zf = scan (columnMetrics s) initialMetrics

initialMetrics :: Vector1 IntN
initialMetrics = replicate 8 (-32678)

columnMetrics :: Vector1 IntN -> Vector1 IntN -> Vector (Data Index, Data Index)
                  -> Vector1 IntN
columnMetrics s prev zf  = zipWith (metricFast prev) zf s

metricFast :: Vector1 IntN -> (Data Index, Data Index) -> Data IntN -> Data IntN
metricFast prev (z, _) _ = prev ! z
-- End one test.

copyPush :: Vector1 Index -> PV.PushVector1 Index
copyPush v = let pv = PV.toPush v in pv PV.++ pv

scanlPush :: PV.PushVector1 WordN -> Vector1 WordN -> PV.PushVector (PV.PushVector1 WordN)
scanlPush = PV.scanl (\a b ->  a )

tests = testGroup "RegressionTests"
    [ mkGoldTest example9 "example9" defaultOptions
    , mkGoldTest pairParam "pairParam" defaultOptions
    , mkGoldTest pairParam2 "pairParam2" defaultOptions
    , mkGoldTest topLevelConsts "topLevelConsts" defaultOptions
    , mkGoldTest topLevelConsts "topLevelConsts_native" nativeOpts
    , mkGoldTest metrics "metrics" defaultOptions
    , mkGoldTest scanlPush "scanlPush" defaultOptions
    , mkBuildTest pairParam "pairParam" defaultOptions
    , mkBuildTest topLevelConsts "topLevelConsts" defaultOptions
    , mkBuildTest topLevelConsts "topLevelConsts_native" nativeOpts
    , mkBuildTest metrics "metrics" defaultOptions
    , mkBuildTest copyPush "copyPush" defaultOptions
    , mkBuildTest scanlPush "scanlPush" defaultOptions
    ]

main = defaultMain [tests]


-- Helper functions
nativeOpts = defaultOptions{rules=nativeArrayRules}

writeGoldFile fun name opts = compile fun ("tests/gold/" <> name) name opts

mkGoldTest fun name opts = goldenVsFile name ("tests/gold/" <> name <> ".c") ("tests/" <> name <> ".c")
                         $ compile fun ("tests/" <> name) name opts

ghc = command_ (decodeString "ghc") [ pack "-c"
                                    , pack "-optc -Ilib/Feldspar/C"
                                    , pack "-optc -std=c99"
                                    , pack "-Wall"
                                    ]

mkBuildTest fun name opts = testCase name $ do let base  = "tests/" <> name <> "_build_test"
                                                   cfile = base <> ".c"
                                               compile fun base name opts
                                               shellyNoDir $ ghc [pack cfile]


