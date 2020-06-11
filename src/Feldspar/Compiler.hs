module Feldspar.Compiler
    ( compile
    , compileUT
    , icompile
    , icompileWith
    , icompile'
    , program
    , programOpts
    , programOptsArgs
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
    ) where

import Feldspar.Compiler.Internal
import Feldspar.Core.Interpretation (FeldOpts(..), Target(..))

