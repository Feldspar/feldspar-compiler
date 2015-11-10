{-# LANGUAGE CPP #-}

module Feldspar.Compiler
    ( compile
    , icompile
    , icompileWith
    , icompile'
    , getCore
    , printCore
    , Options (..)
    , defaultOptions
    , sicsOptions
    , sicsOptions2
    , sicsOptions3
    , FeldOpts(..)
    , Target(..)
    , c99PlatformOptions
    , c99OpenMpPlatformOptions
    , tic64xPlatformOptions
    , feldsparCIncludes
    ) where

import System.FilePath

import Feldspar.Compiler.Internal
import Feldspar.Core.Interpretation (FeldOpts(..), Target(..))

#ifdef CABAL_IS_USED

import Paths_feldspar_compiler

-- | Get the path to the C include files installed by Cabal
feldsparCIncludes :: IO FilePath
feldsparCIncludes = fmap (</> "include") getLibDir

#else

import System.Directory

-- | Get the path to the C include files
--
-- It returns a platform-independent representation of the directory
-- @../feldspar-compiler/lib/Feldspar/C@. This assumes that the method is called
-- either from the root of the @feldspar-compiler@ repository, or from a
-- different directory located next to @feldspar-compiler@.
feldsparCIncludes :: IO FilePath
feldsparCIncludes = do
    dir <- getCurrentDirectory
    return $ case splitPath dir of
        [] -> dir
        ds -> joinPath $ init ds ++ ["feldspar-compiler","clib"]

#endif

