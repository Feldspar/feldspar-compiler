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

module Feldspar.Compiler.Frontend.CommandLine.API.Options where

import qualified Feldspar.Compiler.Backend.C.Options as CompilerCoreOptions
import qualified Feldspar.Compiler.Compiler as CompilerCore
import qualified Feldspar.Compiler.Frontend.CommandLine.API.Library as StandaloneLib
import Feldspar.Compiler.Backend.C.Platforms

import Data.List
import Data.Char
import Data.Maybe (fromMaybe)

import System.Console.GetOpt
import System.Exit
import System.IO

availablePlatformsStrRep :: String
availablePlatformsStrRep = StandaloneLib.formatStringList $
                              map (StandaloneLib.upperFirst . CompilerCoreOptions.name) availablePlatforms

data StandaloneMode = SingleFunction String | MultiFunction

data FrontendOptions = FrontendOptions
                       { optStandaloneMode     :: StandaloneMode
                       , optOutputFileName     :: Maybe String
                       , optCompilerMode       :: CompilerCoreOptions.Options
                       }

-- | Default options
startOptions :: FrontendOptions
startOptions = FrontendOptions
                { optStandaloneMode = MultiFunction
                , optOutputFileName = Nothing
                , optCompilerMode   = CompilerCore.defaultOptions
                }

helpHeader :: String
helpHeader = "Standalone Feldspar Compiler\nUsage: feldspar [options] inputfile\n" ++
         "Notes: \n" ++
         " * When no output file name is specified, the input file's name with .c extension is used\n" ++
         " * The inputfile parameter is always needed, even in single-function mode\n" ++
         "\nAvailable options: \n"

-- | Option descriptions for getOpt
optionDescriptors :: [ OptDescr (FrontendOptions -> IO FrontendOptions) ]
optionDescriptors =
    [ Option "f" ["singlefunction"]
        (ReqArg
            (\arg opt -> return opt { optStandaloneMode = SingleFunction arg })
            "FUNCTION")
        "Enables single-function compilation"
    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutputFileName = Just arg })
            "outputfile")
        "Overrides the file names for the generated output code"

    , Option "p" ["platform"]
        (ReqArg
            (\arg opt -> return opt { optCompilerMode = (optCompilerMode opt)
                                         { CompilerCoreOptions.platform = decodePlatform arg } })
            "<platform>")
        ("Overrides the target platform " ++ availablePlatformsStrRep)
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                --prg <- getProgName
                hPutStrLn stderr (usageInfo helpHeader optionDescriptors)
                exitSuccess))
        "Show this help message"
    ]

-- ==============================================================================
--  == Option Decoders
-- ==============================================================================

findPlatformByName :: String -> Maybe CompilerCoreOptions.Platform
findPlatformByName platformName = -- Finds a platform by name using case-insensitive comparison
    find (\platform -> map toLower platformName == map toLower (CompilerCoreOptions.name platform))
         availablePlatforms

decodePlatform :: String -> CompilerCoreOptions.Platform
decodePlatform s = fromMaybe (error $ "Invalid platform specified. Valid platforms are: " ++ availablePlatformsStrRep)
                 $ findPlatformByName s

parseInt :: String -> String -> Int
parseInt arg message = case reads arg of
    [(x, "")] -> x
    _ -> error message
