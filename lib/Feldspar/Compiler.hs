{-# LANGUAGE ForeignFunctionInterface #-}

module Feldspar.Compiler
    ( compile
    , icompile
    , icompileWith
    , icompile'
    , getCore
    , printCore
    , Options (..)
    , defaultOptions
    , FeldOpts(..)
    , Target(..)
    , c99PlatformOptions
    , c99OpenMpPlatformOptions
    , tic64xPlatformOptions
    , nativeArrayRules
    , unrollOptions
    , noPrimitiveInstructionHandling
    , noMemoryInformation
    , feldspar_compiler_hook
    ) where

import Feldspar.Compiler.Internal
import Feldspar.Core.Interpretation (FeldOpts(..), Target(..))

feldspar_compiler_hook :: Int
feldspar_compiler_hook = sum [ feldspar_c99_hook
                             , feldspar_ivar_hook
                             , feldspar_taskpool_hook
                             ]

foreign import ccall safe "feldspar_c99_hook"
  feldspar_c99_hook :: Int

foreign import ccall safe "feldspar_ivar_hook"
  feldspar_ivar_hook :: Int

foreign import ccall safe "feldspar_taskpool_hook"
  feldspar_taskpool_hook :: Int
