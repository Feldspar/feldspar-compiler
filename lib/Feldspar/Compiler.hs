
module Feldspar.Compiler
    ( compile
    , compileFile
    , icompile
    , icompileWith
    , icompile'
    , icompileFile
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
    ) where

import Feldspar.Compiler.Internal
import Feldspar.Core.Interpretation (FeldOpts(..), Target(..))

