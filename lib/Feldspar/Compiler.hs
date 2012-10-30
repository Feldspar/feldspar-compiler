module Feldspar.Compiler
    ( compile
    , icompile
    , Options (..)
    , defaultOptions
    , c99PlatformOptions
    , tic64xPlatformOptions
    , unrollOptions
    , noPrimitiveInstructionHandling
    , noMemoryInformation
    ) where

import Feldspar.Compiler.Internal

