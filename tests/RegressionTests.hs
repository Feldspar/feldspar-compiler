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
import Feldspar.Compiler.ExternalProgram (compileFile)
import Feldspar.Vector

import Control.Monad
import Control.Monad.Error (liftIO)
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Search as LB
import System.Process
import Text.Printf

example9 :: Data Int32 -> Data Int32
example9 a = condition (a<5) (3*(a+20)) (30*(a+20))

-- Compile and load example9 as c_example9 (using plugins)
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
metrics :: Pull1 IntN -> Pull1 IntN
            -> Pull DIM1 (Pull DIM1 (Data Index, Data Index)) -> Pull DIM1 (Pull1 IntN)
metrics s _ = scan (columnMetrics s) initialMetrics

initialMetrics :: Pull1 IntN
initialMetrics = replicate1 8 (-32678)

columnMetrics :: Pull1 IntN -> Pull1 IntN -> Pull DIM1 (Data Index, Data Index)
                  -> Pull1 IntN
columnMetrics s prev zf  = zipWith (metricFast prev) zf s

metricFast :: Pull1 IntN -> (Data Index, Data Index) -> Data IntN -> Data IntN
metricFast prev (z, _) _ = prev !! z
-- End one test.

copyPush :: Pull1 Index -> DPush DIM1 Index
copyPush v = let pv = toPush v in pv ++ pv

-- scanlPush :: DPush sh WordN -> Pull1 WordN -> Push (DPush WordN)
-- scanlPush = scanl const

concatV :: Pull DIM1 (Pull1 IntN) -> DPush DIM1 IntN
concatV xs = fromZero $ fold (++) empty xs

complexWhileCond :: Data Int32 -> (Data Int32, Data Int32)
complexWhileCond y = whileLoop (0,y) (\(a,b) -> ((\a b -> a * a /= b * b) a (b-a))) (\(a,b) -> (a+1,b))

-- One test starting
divConq3 :: Pull DIM1 (Data IntN) -> DPush DIM1 IntN
divConq3 xs = concatV $ pmap (map (+1)) (segment 1024 xs)

pmap :: (Syntax a, Syntax b) => (a -> b) -> Pull DIM1 a -> Pull DIM1 b
pmap f = map await . force . map (future . f)

segment :: Syntax a => Data Length -> Pull DIM1 a -> Pull DIM1 (Pull DIM1 a)
segment l xs = indexed1 clen (\ix -> take l $ drop (ix*l) xs)
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

arrayInStruct :: Data [Length] -> Data [Length]
arrayInStruct a = snd $ whileLoop (getLength a, a) (\(n,_) -> (n>0)) (\(n,a) -> (n-1, parallel (getLength a) (\ i -> a!i + 5)))

arrayInStructInStruct :: Data (Length, (Length, [Length])) -> Data (Length, (Length, [Length]))
arrayInStructInStruct x = x

fut1 :: Future (Data IntN) -> Future (Data IntN)
fut1 x  = forLoop 20 x (\_ e -> future $ force $ await e)

tests :: TestTree
tests = testGroup "RegressionTests" [compilerTests, externalProgramTests]

compilerTests :: TestTree
compilerTests = testGroup "Compiler-RegressionTests"
    [ testProperty "example9 (plugin)" $ eval example9 ==== c_example9
    , mkGoldTest example9 "example9" defaultOptions
    , mkGoldTest pairParam "pairParam" defaultOptions
    , mkGoldTest pairParam2 "pairParam2" defaultOptions
    , mkGoldTest concatV "concatV" defaultOptions
    , mkGoldTest complexWhileCond "complexWhileCond" defaultOptions
    , mkGoldTest topLevelConsts "topLevelConsts" defaultOptions
    , mkGoldTest topLevelConsts "topLevelConsts_native" nativeOpts
    , mkGoldTest topLevelConsts "topLevelConsts_sics" sicsOpts
    , mkGoldTest metrics "metrics" defaultOptions
--    , mkGoldTest scanlPush "scanlPush" defaultOptions
    , mkGoldTest divConq3 "divConq3" defaultOptions
    , mkGoldTest ivartest "ivartest" defaultOptions
    , mkGoldTest ivartest2 "ivartest2" defaultOptions
    , mkGoldTest arrayInStruct "arrayInStruct" defaultOptions
    , mkGoldTest arrayInStructInStruct "arrayInStructInStruct" defaultOptions
    , mkGoldTest fut1 "fut1" defaultOptions
   -- Build tests.
    , mkBuildTest pairParam "pairParam" defaultOptions
    , mkBuildTest concatV "concatV" defaultOptions
    , mkBuildTest topLevelConsts "topLevelConsts" defaultOptions
    , mkBuildTest topLevelConsts "topLevelConsts_native" nativeOpts
    , mkBuildTest topLevelConsts "topLevelConsts_sics" sicsOpts
    , mkBuildTest metrics "metrics" defaultOptions
    , mkBuildTest copyPush "copyPush" defaultOptions
--    , mkBuildTest scanlPush "scanlPush" defaultOptions
    , mkBuildTest divConq3 "divConq3" defaultOptions
    , testProperty "bindToThen" (\y -> eval bindToThen y === y)
    , mkGoldTest switcher "switcher" defaultOptions
    , mkBuildTest ivartest "ivartest" defaultOptions
    , mkBuildTest ivartest2 "ivartest2" defaultOptions
    , mkBuildTest arrayInStruct "arrayInStruct" defaultOptions
    , mkBuildTest arrayInStructInStruct "arrayInStructInStruct" defaultOptions
    , mkBuildTest fut1 "fut1" defaultOptions
    ]

externalProgramTests :: TestTree
externalProgramTests = testGroup "ExternalProgram-RegressionTests"
    [ mkParseTest "example9" defaultOptions
    , mkParseTest "pairParam" defaultOptions
    , mkParseTest "pairParam2" defaultOptions
    , mkParseTest "concatV" defaultOptions
    , mkParseTest "complexWhileCond" defaultOptions
    , mkParseTest "topLevelConsts" defaultOptions
    , mkParseTest "topLevelConsts_native" nativeOpts
    , mkParseTest "topLevelConsts_sics" sicsOpts
    -- TODO: Enable when encodeType does not include sizes in struct names.
--    , mkParseTest "metrics" defaultOptions
--    , mkParseTest "scanlPush" defaultOptions
    -- Still incomplete reconstruction of futures.
    , mkParseTest "divConq3" defaultOptions
    , mkParseTest "switcher" defaultOptions
    , mkParseTest "ivartest" defaultOptions
    , mkParseTest "ivartest2" defaultOptions
    , mkParseTest "arrayInStruct" defaultOptions
    , mkParseTest "arrayInStructInStruct" defaultOptions
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

mkParseTest n opts = do
    let ref = goldDir <> n
        new = testDir <> "ep-" <> n
        act = compileFile ref new opts
        cmp = fuzzyCmp $ printf "Files '%s' and '%s' differ" ref new
        upd = LB.writeFile ref
    goldenTest n (vgReadFiles ref) (liftIO act >> vgReadFiles new) cmp upd

fuzzyCmp :: String -> LB.ByteString -> LB.ByteString -> IO (Maybe String)
fuzzyCmp e x y =
  return $ if x Prelude.== (filterEp y) then Nothing else Just e

-- Removes "EP-"-related prefixes from the generated output.
filterEp :: LB.ByteString -> LB.ByteString
filterEp xs = LB.replace (B.pack "TESTS_EP-") (B.pack "TESTS_") xs'
  where xs' = LB.replace (B.pack "#include \"ep-") (B.pack "#include \"") xs

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
