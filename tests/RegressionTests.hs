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
import qualified Data.Text.Lazy as LT
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

concatV :: Vector (Vector1 IntN) -> Vector1 IntN
concatV = fold (++) Empty

complexWhileCond :: Data Int32 -> (Data Int32, Data Int32)
complexWhileCond y = whileLoop (0,y) (\(a,b) -> ((\a b -> a * a /= b * b) a (b-a))) (\(a,b) -> ((a+1),b))

-- One test starting
divConq3 :: Vector1 IntN -> Vector1 IntN
divConq3 xs = concatV $ pmap (map (+1)) (segment 1024 xs)

pmap :: (Syntax a, Syntax b) => (a -> b) -> Vector a -> Vector b
pmap f = map await . force . map (future . f)

segment :: Syntax a => Data Length -> Vector a -> Vector (Vector a)
segment l xs = indexed clen (\ix -> (take l $ drop (ix*l) xs))
  where clen = (length xs) `div` l
-- End one test.

tests = testGroup "RegressionTests"
    [ mkGoldTest example9 "example9" defaultOptions
    , mkGoldTest pairParam "pairParam" defaultOptions
    , mkGoldTest pairParam2 "pairParam2" defaultOptions
    , mkGoldTest concatV "concatV" defaultOptions
    , mkGoldTest complexWhileCond "complexWhileCond" defaultOptions
    , mkGoldTest topLevelConsts "topLevelConsts" defaultOptions
    , mkGoldTest topLevelConsts "topLevelConsts_native" nativeOpts
    , mkGoldTest topLevelConsts "topLevelConsts_sics" sicsOpts
    , mkGoldTest metrics "metrics" defaultOptions
    , mkGoldTest scanlPush "scanlPush" defaultOptions
    , mkGoldTest divConq3 "divConq3" defaultOptions
    , mkBuildTest pairParam "pairParam" defaultOptions
    , mkBuildTest concatV "concatV" defaultOptions
    , mkBuildTest topLevelConsts "topLevelConsts" defaultOptions
    , mkBuildTest topLevelConsts "topLevelConsts_native" nativeOpts
    , mkBuildTest topLevelConsts "topLevelConsts_sics" sicsOpts
    , mkBuildTest metrics "metrics" defaultOptions
    , mkBuildTest copyPush "copyPush" defaultOptions
    , mkBuildTest scanlPush "scanlPush" defaultOptions
    , mkBuildTest divConq3 "divConq3" defaultOptions
    ]

main = defaultMain [tests]


-- Helper functions
jointSuffix = "tmp"
testDir = "tests/"
goldDir = "tests/gold/"

nativeOpts = defaultOptions{rules=nativeArrayRules}
sicsOpts = defaultOptions{frontendOpts=defaultFeldOpts{targets= [SICS]}}

writeGoldFile fun name opts = compile fun (goldDir <> name) name opts

mkGoldTest fun name opts = buildTest $ do
    compile fun (testDir <> name) name opts
    shellyNoDir $ mkJointFile (pack testDir) name
    shellyNoDir $ mkJointFile (pack goldDir) name
    return $ goldenVsFile name (goldDir <> name <> "." <> jointSuffix) (testDir <> name <> "." <> jointSuffix) $ return ()

ghc = command_ (decodeString "ghc") [ pack "-c"
                                    , pack "-optc -Ilib/Feldspar/C"
                                    , pack "-optc -std=c99"
                                    , pack "-Wall"
                                    ]
-- | Creates a temporary file that is used for the comparison by
-- concatenating the header file and c file into a single temporary file.
mkJointFile base name = do
     testH <- readfile (base </> name <.> pack "h")
     testC <- readfile (base </> name <.> pack "c")
     writefile (base </> name <.> pack jointSuffix) (LT.append testH testC)

mkBuildTest fun name opts = testCase name $ do let base  = testDir <> name <> "_build_test"
                                                   cfile = base <> ".c"
                                               compile fun base name opts
                                               shellyNoDir $ ghc [pack cfile]


