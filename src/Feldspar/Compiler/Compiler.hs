--
-- Copyright (c) 2009-2011, ERICSSON AB
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Feldspar.Compiler.Compiler (
    compileToCCore
  , compileToCCore'
  , defaultOptions
  , sicsOptions
  , sicsOptions2
  , sicsOptions3
  , c99PlatformOptions
  , c99OpenMpPlatformOptions
  , tic64xPlatformOptions
  , SplitModule(..)
  , CompiledModule(..)
  , program
  , programOpts
  , programOptsArgs
  ) where

import Data.List (partition)
import Data.Maybe (fromMaybe)

import Feldspar.Core.Constructs (SyntacticFeld)
import Feldspar.Core.Interpretation (defaultFeldOpts, FeldOpts(..), Target(..))
import Feldspar.Core.Frontend (reifyFeld)
import Feldspar.Core.UntypedRepresentation (UntypedFeld, VarId)
import Feldspar.Core.Types (BitWidth(N32))
import Feldspar.Core.Middleend.FromTyped
import Feldspar.Compiler.Backend.C.Library
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Backend.C.Platforms
import Feldspar.Compiler.Backend.C.CodeGeneration
import Feldspar.Compiler.Backend.C.MachineLowering
import Feldspar.Compiler.Backend.C.Tic64x
import Feldspar.Compiler.Imperative.FromCore
import Feldspar.Compiler.Imperative.Representation
import Feldspar.Core.Middleend.PassManager
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt
import System.IO
import Control.Monad (when)

data SplitModule = SplitModule
    { implementation :: CompiledModule
    , interface :: CompiledModule
    }

data CompiledModule = CompiledModule {
    sourceCode      :: String,
    debugModule     :: Module ()
}

-- | Split a module into interface and implemenation.
splitModule :: Module () -> (Module (), Module ())
splitModule m = (Module (hdr ++ createProcDecls (entities m)), Module body)
  where
    (hdr, body) = partition belongsToHeader (entities m)
    belongsToHeader :: Entity () -> Bool
    belongsToHeader StructDef{}                     = True
    belongsToHeader Proc{..} | Nothing <- procBody  = True
    belongsToHeader _                               = False
    -- TODO These only belongs in the header iff the types are used in a
    -- function interface
    createProcDecls :: [Entity ()] -> [Entity ()]
    createProcDecls = concatMap defToDecl
    defToDecl :: Entity () -> [Entity ()]
    defToDecl (Proc n False inp outp _) = [Proc n False inp outp Nothing]
    defToDecl _ = []

compileSplitModule :: Options -> (Module (), Module ()) -> SplitModule
compileSplitModule opts (hmdl, cmdl)
  = SplitModule
    { interface = CompiledModule { sourceCode  = incls ++ hres
                                 , debugModule = hmdl
                                 }
    , implementation = CompiledModule { sourceCode  = cres
                                      , debugModule = cmdl
                                      }
    }
  where
    hres = compToCWithInfos opts hmdl
    cres = compToCWithInfos opts cmdl
    incls = genIncludeLines opts Nothing

-- | Compiler core.
-- Everything should call this function and only do a trivial interface adaptation.
-- Do not duplicate.
compileToCCore :: SyntacticFeld c => String -> Options -> c -> SplitModule
compileToCCore name opts prg = compileToCCore' opts mod
      where
        mod = fromCore opts (encodeFunctionName name) prg

compileToCCore' :: Options -> Module () -> SplitModule
compileToCCore' opts m = compileSplitModule opts $ splitModule mod
      where
        mod = adaptTic64x opts $ rename opts False m

genIncludeLines :: Options -> Maybe String -> String
genIncludeLines opts mainHeader = concatMap include incs ++ "\n\n"
  where
    include []            = ""
    include fname@('<':_) = "#include " ++ fname ++ "\n"
    include fname         = "#include \"" ++ fname ++ "\"\n"
    incs = includes (platform opts) ++ [fromMaybe "" mainHeader]

-- | Predefined options

defaultOptions :: Options
defaultOptions
    = Options
    { platform          = c99
    , printHeader       = False
    , useNativeArrays   = False
    , useNativeReturns  = False
    , frontendOpts      = defaultFeldOpts
    , safetyLimit       = 2000
    , nestSize          = 2
    }

c99PlatformOptions :: Options
c99PlatformOptions              = defaultOptions

c99OpenMpPlatformOptions :: Options
c99OpenMpPlatformOptions        = defaultOptions { platform = c99OpenMp }

tic64xPlatformOptions :: Options
tic64xPlatformOptions           = defaultOptions { platform = tic64x }

sicsOptions :: Options
sicsOptions = defaultOptions { frontendOpts = defaultFeldOpts { targets = [SICS,CSE] }}

sicsOptions2 :: Options
sicsOptions2 = defaultOptions { frontendOpts = defaultFeldOpts { targets = [SICS] }}

sicsOptions3 :: Options
sicsOptions3 = defaultOptions { platform = c99Wool, frontendOpts = defaultFeldOpts { targets = [SICS,CSE,Wool] }}

data BackendPass = BPFromCore
                 | BPRename
                 | BPAdapt
                 | BPSplit
                 | BPCompile
                 | BPUnsplit
  deriving (Eq, Enum, Bounded, Read, Show)

data ProgOpts =
    ProgOpts
    { backOpts     :: Options
    , passFileName :: String
    , outFileName  :: String
    , functionName :: String
    , frontendCtrl :: PassCtrl FrontendPass
    , backendCtrl  :: PassCtrl BackendPass
    , printHelp    :: Bool
    }

defaultProgOpts :: ProgOpts
defaultProgOpts =
    ProgOpts
    { backOpts     = defaultOptions
    , passFileName = ""
    , outFileName  = ""
    , functionName = ""
    , frontendCtrl = defaultPassCtrl
    , backendCtrl  = defaultPassCtrl
    , printHelp    = False
    }

program :: SyntacticFeld a => a -> IO ()
program p = programOpts p defaultOptions

programOpts :: SyntacticFeld a => a -> Options -> IO ()
programOpts p opts = do args <- getArgs
                        programOptsArgs p defaultProgOpts{backOpts = opts} args

programOptsArgs :: SyntacticFeld a => a -> ProgOpts -> [String] -> IO ()
programOptsArgs p opts args = programComp (const (return p)) opts args

programComp :: SyntacticFeld a => ([String] -> IO a) -> ProgOpts -> [String] -> IO ()
programComp pc opts args = do name <- getProgName
                              let (opts1,nonopts) = decodeOpts (optsFromName opts name) args
                              let header = "Usage: " ++ name ++ " <option>...\n"
                                         ++ "where <option> is one of"
                              if printHelp opts1
                                then putStr $ usageInfo header optionDescs ++ passInfo
                                else
                                  do p <- pc nonopts
                                     let (strs,mProgs) = translate opts1 p
                                     when (not $ null strs)
                                        $ writeFileLB (passFileName opts1) (concat strs)
                                     case mProgs of
                                       Nothing -> return ()
                                       Just progs -> mapM_ (uncurry writeFileLB)
                                                   $ zip (outFileNames opts1)
                                                   $ targetCode progs

optsFromName :: ProgOpts -> String -> ProgOpts
optsFromName opts name = opts{passFileName = name ++ ".passes",
                              outFileName = name,
                              functionName = name}

outFileNames :: ProgOpts -> [String]
outFileNames opts = [name ++ ".h", name ++ ".c"]
  where name = outFileName opts

decodeOpts :: ProgOpts -> [String] -> (ProgOpts, [String])
decodeOpts optsIn argv = if null errors then (foldl (\ o f -> f o) optsIn actions, nonOptions)
                                        else error $ unlines errors
   where (actions, nonOptions, errors) = getOpt Permute optionDescs argv

passInfo :: String
passInfo = "\nPASS is a frontend pass from\n" ++
           (unlines $ map ((++) "  " . unwords) $ chunksOf 5 $ map show [minBound .. maxBound :: FrontendPass]) ++
           "or a backend pass from\n" ++
           (unlines $ map ((++) "  " . unwords) $ chunksOf 5 $ map show [minBound .. maxBound :: BackendPass])
  where chunksOf n [] = []
        chunksOf n xs = take n xs : chunksOf n (drop n xs)

optionDescs = driverOpts

driverOpts =
  [ Option []  ["writeBefore"] (ReqArg (chooseEnd addWrBefore) "PASS")   "write IR before PASS"
  , Option []  ["writeAfter"]  (ReqArg (chooseEnd addWrAfter) "PASS")    "write IR after PASS"
  , Option []  ["stopBefore"]  (ReqArg (chooseEnd setStopBefore) "PASS") "stop processing before PASS"
  , Option []  ["stopAfter"]   (ReqArg (chooseEnd setStopAfter) "PASS")  "stop processing after PASS"
  , Option []  ["skip"]        (ReqArg (chooseEnd addSkip) "PASS")       "skip PASS"
  , Option "o" ["outFile"]     (ReqArg (\ arg opts -> opts{outFileName = arg}) "FILE") "set base name of out file"
  , Option "p" ["passFile"]    (ReqArg (\ arg opts -> opts{passFileName = arg}) "FILE") "set name of pass file"
  , Option []  ["funcName"]    (ReqArg (\ arg opts -> opts{functionName = arg}) "IDENTIFIER") "set name of generated function"
  , Option "h" ["help"]        (NoArg (\ opts -> opts{printHelp = True})) "print a useage message"
  ]

chooseEnd :: (forall a . PassCtrl a -> a -> PassCtrl a) -> String -> ProgOpts -> ProgOpts
chooseEnd f str opts
  | [(p,_)] <- reads str = opts{frontendCtrl = f (frontendCtrl opts) p}
  | [(p,_)] <- reads str = opts{backendCtrl = f (backendCtrl opts) p}
  | otherwise = error $ "Compiler.chooseEnd: unrecognized pass " ++ str

writeFileLB :: String -> String -> IO ()
writeFileLB  "-" str = putStr str
writeFileLB name str = do fh <- openFile name WriteMode
                          hSetBuffering fh LineBuffering
                          hPutStr fh str
                          hClose fh

translate :: SyntacticFeld a => ProgOpts -> a -> ([String], Maybe TargetCode)
translate opts p = (ssf ++ ssb, as)
  where astf = reifyFeld fopts N32 p
        (ssf,ut) = frontend (frontendCtrl opts) fopts astf
        (ssb,as) = maybe ([], Nothing) (backend (backendCtrl opts) bopts name) ut
        bopts = backOpts opts
        fopts = frontendOpts bopts
        name = functionName opts

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (x,y) = "(" ++ pretty x ++ ", " ++ pretty y ++ ")"

instance Pretty (Module ()) where
  pretty m = compToCWithInfos defaultOptions m

instance Pretty VarId where
  pretty v = show v

instance Pretty TargetCode where
  pretty (TargetCode bs) = concat ["// Block\n" ++ b ++ "\n" | b <- bs]

instance Pretty SplitModule where
  pretty (SplitModule impl intf) = "// Interface\n" ++ sourceCode intf ++
                                   "\n// Implementation\n" ++ sourceCode impl

backend :: PassCtrl BackendPass -> Options -> String -> UntypedFeld -> ([String], Maybe TargetCode)
backend ctrl opts name = evalPasses 0
                       $ pt BPUnsplit  unsplit
                       . pt BPCompile  (compileSplitModule opts)
                       . pt BPSplit    splitModule
                       . pc BPAdapt    (adaptTic64x opts)
                       . pc BPRename   (rename opts False)
                       . pt BPFromCore (fst . fromCoreUT opts (encodeFunctionName name))
  where pc :: Pretty a => BackendPass -> (a -> a) -> Prog a Int -> Prog a Int
        pc = passC ctrl
        pt :: (Pretty a, Pretty b) => BackendPass -> (a -> b) -> Prog a Int -> Prog b Int
        pt = passT ctrl

data TargetCode = TargetCode {targetCode :: [String]}

unsplit :: SplitModule -> TargetCode
unsplit (SplitModule impl intf) = TargetCode [sourceCode intf, sourceCode impl]
