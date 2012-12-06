module Feldspar.Compiler
    ( compile
    , icompile
    , getCore
    , printCore
    , Options (..)
    , defaultOptions
    , c99PlatformOptions
    , tic64xPlatformOptions
    , nativeArrayRules
    , unrollOptions
    , noPrimitiveInstructionHandling
    , noMemoryInformation
    ) where

import Feldspar.Compiler.Internal

