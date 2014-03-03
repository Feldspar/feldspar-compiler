{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Dynamically load a compiled Feldspar function as a Haskell function
module Feldspar.Compiler.Plugin
  ( loadFun
  , loadFunOpts
  , loadFunWithConfig
  , defaultConfig
  , pack   -- from MultiStage
  , unpack -- from MultiStage
  )
  where

import System.Plugins (initLinker, loadRawObject, resolveObjs)
import System.Plugins.MultiStage

import Feldspar.Compiler.CallConv (rewriteType, buildCType, buildHaskellType)

import Data.Word (Word8)
import Foreign.Ptr
import Foreign.Marshal (alloca,pokeArray)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Storable (Storable(..))
import Foreign.C.String (CString, withCString)

import Control.Monad (join, (>=>), when, unless)
import Control.Applicative

import Language.Haskell.TH

import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.Process (readProcessWithExitCode)
import System.Info (os)


-- Feldspar specific
import Feldspar.Runtime
import Feldspar.Compiler (compile, defaultOptions)
import Feldspar.Compiler.Backend.C.Library (encodeFunctionName)
import Feldspar.Compiler.Marshal ()

-- | Default configuration for the loader
feldsparPluginConfig :: Config
feldsparPluginConfig =
    defaultConfig { builder      = feldsparBuilder
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

-- | Call @loadFun@ with C compiler options
loadFunOpts :: [String] -> Name -> Q [Dec]
loadFunOpts o = loadFunWithConfig feldsparPluginConfig{opts = o}

feldsparWorker :: Name -> [Name] -> Q Body
feldsparWorker fun as = normalB
    [|calloca $ \outPtr -> do
        join $(infixApp (apply ([|pure $(varE fun)|] : map toRef as)) [|(<*>)|] [|pure outPtr|])
        peek outPtr >>= unpack
    |]
  where
    toRef name = [| pack $(varE name) |]

    apply :: [ExpQ] -> ExpQ
    apply [] = error "apply []"
    apply [x] = x
    apply (x:y:zs) = apply (infixApp x [|(<*>)|] y : zs)

calloca :: forall a b. Storable a => (Ptr a -> IO b) -> IO b
calloca f = alloca $ \ptr -> do
              pokeArray (castPtr ptr) $ replicate (sizeOf ptr) (0::Word8)
              f ptr

feldsparBuilder :: Config -> Name -> Q Body
feldsparBuilder Config{..} fun = normalB
    [|unsafeLocalState $ do
        createDirectoryIfMissing True wdir
        $(varE 'compile) $(varE fun) basename base defaultOptions
        compileAndLoad basename opts
        lookupSymbol symbol
    |]
  where
    base     = nameBase fun
    basename = wdir ++ "/" ++ base
    symbol   = ldprefix ++ encodeFunctionName base
    ldprefix = case os of
                 "darwin" -> "_"
                 _        -> ""

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
    let args = [ "-package feldspar-compiler"
               , "-optc -std=c99"
               , "-optc -Wall"
               , "-w"
               , "-c"
               ]
    (_,stdout,stderr) <- readProcessWithExitCode "ghc" (args ++ opts ++ ["-o",objfile,srcfile]) ""
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

