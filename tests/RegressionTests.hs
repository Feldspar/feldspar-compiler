{-# LANGUAGE TemplateHaskell #-}

module Main where

-- To generate the golden files use a script similiar to this one
-- > ghc -ilib -isrc -itests tests/RegressionTests.hs -e 'writeGoldFile example9 "example9" defaultOptions'
-- > ghc -ilib -isrc -itests tests/RegressionTests.hs -e 'writeGoldFile example9 "example9_native" nativeOpts'

import Test.Tasty
import Test.Tasty.Golden.Advanced
import Test.Tasty.QuickCheck

import qualified Prelude
import Feldspar
import Feldspar.Compiler
import Feldspar.Compiler.Plugin
import Feldspar.Vector
import Feldspar.Vector.Internal (scan)
import qualified Feldspar.Vector.Push as PV

import Control.Monad
import Control.Monad.Error (liftIO)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as LB
import System.Process
import Text.Printf

example9 :: Data Int32 -> Data Int32
example9 a = condition (a<5) (3*(a+20)) (30*(a+20))

-- Compile and load example9 as c_example9
loadFun 'example9

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
metrics s _ = scan (columnMetrics s) initialMetrics

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
scanlPush = PV.scanl const

concatV :: Vector (Vector1 IntN) -> Vector1 IntN
concatV = fold (++) Empty

complexWhileCond :: Data Int32 -> (Data Int32, Data Int32)
complexWhileCond y = whileLoop (0,y) (\(a,b) -> ((\a b -> a * a /= b * b) a (b-a))) (\(a,b) -> (a+1,b))

-- One test starting
divConq3 :: Vector1 IntN -> Vector1 IntN
divConq3 xs = concatV $ pmap (map (+1)) (segment 1024 xs)

pmap :: (Syntax a, Syntax b) => (a -> b) -> Vector a -> Vector b
pmap f = map await . force . map (future . f)

segment :: Syntax a => Data Length -> Vector a -> Vector (Vector a)
segment l xs = indexed clen (\ix -> take l $ drop (ix*l) xs)
  where clen = length xs `div` l
-- End one test.

-- | We rewrite `return x >>= \_ -> return y` into `return x >> return y`
--   This test ensures that we can still `return x` in the first action.
bindToThen :: Data Index -> Data Index
bindToThen y = runMutable $ do
    ref <- newRef y
    _ <- getRef ref
    getRef ref

switcher :: Data Word8 -> Data Bool -> Data Word8
switcher i = switch (value 0) [(true,i), (false,2)]

ivartest :: Data Index -> Data Index
ivartest a = share (future (a+1)) $ \a' -> await a' * 2

ivartest2 :: (Data Index, Data Index) -> (Data Index, Data Index)
ivartest2 a = share (future a) $ \a' -> await a'

tests :: TestTree
tests = testGroup "RegressionTests"
    [ testProperty "example9 (plugin)" $ eval example9 === c_example9
    , mkGoldTest example9 "example9" defaultOptions
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
    , mkGoldTest ivartest "ivartest" defaultOptions
    , mkGoldTest ivartest2 "ivartest2" defaultOptions
    , mkBuildTest pairParam "pairParam" defaultOptions
    , mkBuildTest concatV "concatV" defaultOptions
    , mkBuildTest topLevelConsts "topLevelConsts" defaultOptions
    , mkBuildTest topLevelConsts "topLevelConsts_native" nativeOpts
    , mkBuildTest topLevelConsts "topLevelConsts_sics" sicsOpts
    , mkBuildTest metrics "metrics" defaultOptions
    , mkBuildTest copyPush "copyPush" defaultOptions
    , mkBuildTest scanlPush "scanlPush" defaultOptions
    , mkBuildTest divConq3 "divConq3" defaultOptions
    , testProperty "bindToThen" (\y -> eval bindToThen y Prelude.== y)
    , mkGoldTest switcher "switcher" defaultOptions
    , mkBuildTest ivartest "ivartest" defaultOptions
    , mkBuildTest ivartest2 "ivartest2" defaultOptions
    ]

main :: IO ()
main = defaultMain tests


-- Helper functions
testDir, goldDir :: Prelude.FilePath
testDir = "tests/"
goldDir = "tests/gold/"

nativeOpts :: Options
nativeOpts = defaultOptions{rules=nativeArrayRules}
sicsOpts :: Options
sicsOpts   = defaultOptions{frontendOpts=defaultFeldOpts{targets= [SICS]}}

writeGoldFile :: Syntax a => a -> Prelude.FilePath -> Options -> IO ()
writeGoldFile fun n = compile fun (goldDir <> n) n

mkGoldTest fun n opts = do
    let ref = goldDir <> n
        new = testDir <> n
        act = compile fun new n opts
        cmp = simpleCmp $ printf "Files '%s' and '%s' differ" ref new
        upd = LB.writeFile ref
    goldenTest n (vgReadFiles ref) (liftIO act >> vgReadFiles new) cmp upd

simpleCmp :: Prelude.Eq a => String -> a -> a -> IO (Maybe String)
simpleCmp e x y =
  return $ if x Prelude.== y then Nothing else Just e

vgReadFiles :: String -> ValueGetter r LB.ByteString
vgReadFiles base = liftM LB.concat $ mapM (vgReadFile . (base<>)) [".h",".c"]

mkBuildTest fun n opts = do
    let new = testDir <> n <> "_build_test"
        cfile = new <> ".c"
        act = do compile fun new n opts
                 (_,so,se) <- readProcessWithExitCode "ghc" [cfile, "-c", "-optc -Ilib/Feldspar/C", "-optc -std=c99", "-Wall"] ""
                 return $ so <> se
        cmp _ _ = return Nothing
        upd _ = return ()
    goldenTest n (return "") (liftIO act >> return "") cmp upd
