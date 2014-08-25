{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Dynamically load a compiled Feldspar function as a Haskell function
module Feldspar.Compiler.Plugin
  ( loadFun
  , loadFunWith
  , loadFunOpts
  , loadFunOptsWith
  , loadFunWithConfig
  , defaultConfig
  , pack   -- from MultiStage
  , unpack -- from MultiStage
  )
  where

import GHC.Paths (ghc)
import GHC.Word (Word32(..))
import System.Plugins (initLinker, loadRawObject, resolveObjs)
import System.Plugins.MultiStage
import Data.List (isPrefixOf)
import Distribution.Verbosity (silent,verbose)
import Distribution.Simple.Utils (defaultPackageDesc)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)

import Feldspar.Compiler.CallConv (rewriteType, buildCType, buildHaskellType)

import Data.Default
import Foreign.Ptr
import Foreign.Marshal (with)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Storable (Storable(..))
import Foreign.C.String (CString, withCString)

import Control.Monad (join, (>=>), when, unless)
import Control.Applicative

import Language.Haskell.TH hiding (Type, Range)
import Language.Haskell.TH.Syntax (Lift(..))

import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Info (os)
import System.IO.Unsafe (unsafePerformIO)


-- Feldspar specific
import Feldspar.Core.Types (WordN(..))
import Feldspar.Range (Range(..))
import Feldspar.Core.Interpretation (FeldOpts(..), Target(..))
import Feldspar.Core.UntypedRepresentation (Signedness(..), Size(..))
import Feldspar.Runtime
import Feldspar.Compiler (compile, defaultOptions)
import Feldspar.Compiler.Imperative.Representation (Type(..), ScalarType(..), Constant(..))
import Feldspar.Compiler.Backend.C.Options (Options(..), Platform(..), ShowValue(..))
import Feldspar.Compiler.Backend.C.Library (encodeFunctionName)
import Feldspar.Compiler.Marshal ()

-- | Configurable configuration for the loader.
feldsparPluginConfigWith :: String -> Options -> Config
feldsparPluginConfigWith suff fopts =
    feldsparPluginConfig { builder = feldsparBuilder suff fopts
                         , prefix = suff
                         }

-- | Default configuration for the loader
feldsparPluginConfig :: Config
feldsparPluginConfig =
    defaultConfig { builder      = feldsparBuilder "" defaultOptions
                  , worker       = feldsparWorker
                  , typeFromName = loadFunType >=> rewriteType
                  , mkHSig       = buildHaskellType
                  , mkCSig       = buildCType
                  }

-- | Compile and load a Feldspar function into the current GHC session.
--
-- > prog1 :: Data Index -> Vector1 Index
-- > prog1 c = indexed c (const c)
-- >
-- > $(loadFun 'prog1)
--
-- The call to @loadFun@ above will splice code into the current module
-- to compile, load and wrap a Feldspar function as a Haskell function:
--
-- > c_prog1 :: Index -> [Index]
--
loadFun :: Name -> Q [Dec]
loadFun = loadFunWithConfig feldsparPluginConfig

-- | @loadFun@ with a function suffix to avoid collisions and different
--  feldspar-compiler options.
loadFunWith :: String -> Options -> Name -> Q [Dec]
loadFunWith s o = loadFunWithConfig (feldsparPluginConfigWith s o)

-- | Call @loadFun@ with C compiler options
loadFunOpts :: [String] -> Name -> Q [Dec]
loadFunOpts o = loadFunWithConfig feldsparPluginConfig{opts = o}

-- | Call @loadFunWith@ with C compiler options
loadFunOptsWith :: String -> Options -> [String] -> Name -> Q [Dec]
loadFunOptsWith s fopt o =
    loadFunWithConfig (feldsparPluginConfigWith s fopt){opts = o}

feldsparWorker :: Name -> [Name] -> Q Body
feldsparWorker fun as = normalB
    [|with def $ \outPtr -> do
        join $(infixApp (apply ([|pure $(varE fun)|] : map toRef as)) [|(<*>)|] [|pure outPtr|])
        peek outPtr >>= from
    |]
  where
    toRef name = [| pack $(varE name) |]

    apply :: [ExpQ] -> ExpQ
    apply [] = error "apply []"
    apply [x] = x
    apply (x:y:zs) = apply (infixApp x [|(<*>)|] y : zs)

feldsparBuilder :: String -> Options -> Config -> Name -> Q Body
feldsparBuilder suffix fopts Config{..} fun = do
    let db    = getDB
    let opts' = opts ++ map ("-I"++) db
    normalB [|unsafeLocalState $ do
                createDirectoryIfMissing True wdir
                $(varE 'compile) $(varE fun) basename base fopts
                compileAndLoad basename opts'
                lookupSymbol symbol
            |]
  where
    base     = nameBase fun ++ suffix
    basename = wdir ++ "/" ++ base
    symbol   = ldprefix ++ encodeFunctionName base
    ldprefix = case os of
                 "darwin" -> "_"
                 _        -> ""

getDB :: [String]
getDB = unsafePerformIO $ do
    dirs <- sequence [ sandbox, user, local ]
    putStrLn $ unwords $ "Using feldspar runtime in" : concat dirs
    return $ concat dirs

  where
    sandbox = do
      (c,d,_) <- readProcessWithExitCode "cabal" ["sandbox", "hc-pkg","field","feldspar-compiler","include-dirs"] ""
      case c of
        ExitSuccess -> return $ drop 1 $ words d
        _           -> return []
    user = do
      (c,d,_) <- readProcessWithExitCode "ghc-pkg" ["field","feldspar-compiler","include-dirs"] ""
      case c of
        ExitSuccess -> return $ drop 1 $ words d
        _           -> return []
    local   = do
      pd <- readPackageDescription verbose =<< defaultPackageDesc verbose
      let f a = return $ includeDirs $ libBuildInfo a
      maybe (return []) f (maybeHasLibs $ flattenPackageDescription pd)
{-# NOINLINE getDB #-}

maybeHasLibs :: PackageDescription -> Maybe Library
maybeHasLibs p =
   library p >>= \lib -> if buildable (libBuildInfo lib)
                           then Just lib
                           else Nothing

compileAndLoad :: String -> [String] -> IO ()
compileAndLoad name opts = do
    let cname = name ++ ".c"
    let oname = name ++ ".o"
    exists <- doesFileExist oname
    when exists $ removeFile oname
    compileC cname oname opts
    initLinker
    _ <- loadRawObject oname
    resolveObjs $ error $ "Symbols in " ++ oname ++ " could not be resolved"

compileC :: String -> String -> [String] -> IO ()
compileC srcfile objfile opts = do
    let args = [ "-optc -std=c99"
               , "-optc -Wall"
               , "-w"
               , "-c"
               ]
    (_,stdout,stderr) <- readProcessWithExitCode ghc (args ++ opts ++ ["-o",objfile,srcfile]) ""
    let output = stdout ++ stderr
    unless (null output) $ putStrLn output

lookupSymbol :: String -> IO (Ptr a)
lookupSymbol symbol = do
    when (0 /= feldspar_compiler_hook) $ error "lookupSymbol: Runtime library missing"
    mptr <- withCString symbol _lookupSymbol
    when (mptr == nullPtr) $ error $ "Symbol " ++ symbol ++ " not found"
    return mptr

foreign import ccall safe "lookupSymbol"
    _lookupSymbol :: CString -> IO (Ptr a)


--- Boring TH instances for Lifting Options -------

instance Lift Options where
    lift (Options platform ph una unr frontopts sl ns) =
        [| Options platform ph una unr frontopts sl ns |]

instance Lift Platform where
    lift (Platform n t vs is vf) = [| Platform n t vs is vf |]

instance Lift FeldOpts where
    lift (FeldOpts ts) = [| FeldOpts ts |]

instance Lift Target where
    lift RegionInf = [| RegionInf |]
    lift Wool      = [| Wool |]
    lift CSE       = [| CSE |]
    lift SICS      = [| SICS |]
    lift BA        = [| BA |]

instance Lift ScalarType where
    lift BoolType        = [| BoolType |]
    lift BitType         = [| BitType |]
    lift FloatType       = [| FloatType |]
    lift DoubleType      = [| DoubleType |]
    lift (NumType sg sz) = [| NumType sg sz |]
    lift (ComplexType t) = [| ComplexType t |]

instance Lift Type where
    lift VoidType             = [| VoidType |]
    lift (MachineVector l st) = [| MachineVector l st |]
    lift (AliasType t s)      = [| AliasType t s |]
    lift (ArrayType r t)      = [| ArrayType r t |]
    lift (NativeArray l t)    = [| NativeArray l t |]
    lift (StructType s es)    = [| StructType s es |]
    lift (Pointer t)          = [| Pointer t |]
    lift (IVarType t)         = [| IVarType t |]

instance Lift Signedness where
    lift Signed   = [| Signed |]
    lift Unsigned = [| Unsigned |]

instance Lift Size where
    lift S8  = [| S8 |]
    lift S16 = [| S16 |]
    lift S32 = [| S32 |]
    lift S40 = [| S40 |]
    lift S64 = [| S64 |]

instance Lift a => Lift (Range a) where
    lift (Range l u) = [| Range l u |]

instance Lift WordN where
    lift (WordN w) = [| WordN w |]

instance Lift Word32 where
    lift x = [| fromInteger $(lift $ toInteger x) :: Word32 |]

instance Lift Float where
  lift x = [| $(litE $ rationalL $ toRational x) :: Float |]

instance Lift Double where
  lift x = [| $(litE $ rationalL $ toRational x) :: Double |]

instance Lift t => Lift (Constant t) where
    lift (IntConst v t)       = [| IntConst v t |]
    lift (DoubleConst v)      = [| DoubleConst v |]
    lift (FloatConst v)       = [| FloatConst v |]
    lift (BoolConst v)        = [| BoolConst v |]
    lift (ComplexConst r i)   = [| ComplexConst r i |]
    lift (ArrayConst vs)      = [| ArrayConst vs |]

instance Lift (Constant () -> String) where
    lift x = [| error "No TH instance for ShowValue" |]
