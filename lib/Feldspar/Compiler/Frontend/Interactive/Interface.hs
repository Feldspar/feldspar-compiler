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

module Feldspar.Compiler.Frontend.Interactive.Interface where

import Feldspar.Compiler.Compiler
import Feldspar.Compiler.Imperative.FromCore
import Feldspar.Compiler.Backend.C.Options
import qualified Feldspar.NameExtractor as NameExtractor
import Feldspar.Compiler.Backend.C.Library
import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Backend.C.Plugin.PrettyPrint
import Feldspar.Compiler.Backend.C.Plugin.Locator

import Data.Char
import System.FilePath (takeBaseName, (<.>))

-- ================================================================================================
--  == Interactive compilation
-- ================================================================================================

data PrgType = ForType | AssignType | IfType

forPrg = ForType
ifPrg  = IfType
assignPrg = AssignType

getProgram :: (Int, Int) -> PrgType -> Module DebugToCSemanticInfo -> IO ()
getProgram (line, col) prgtype prg = res where
     res = if find then putStrLn $ myShow code
                   else putStrLn "Not found appropriate code part!"
     (find, code) = case prgtype of
                        ForType     -> getPrgParLoop (line, col) prg
                        AssignType  -> getPrgAssign (line, col) prg
                        IfType      -> getPrgBranch (line, col) prg


compile :: (Compilable t internal) => t -> FilePath -> String -> Options -> IO ()
compile prg fileName functionName opts = do
    writeFile cfile $ unlines [ "#include \"" ++ takeBaseName fileName <.> "h" ++ "\""
                              , sourceCode $ sctccrSource compilationResult
                              ]
    writeFile hfile $ withIncludeGuard $ sourceCode $ sctccrHeader compilationResult
  where
    hfile = makeHFileName fileName
    cfile = makeCFileName fileName
    compilationResult = compileToCCore Interactive prg
                                       (NameExtractor.OriginalFunctionSignature functionName []) opts

    withIncludeGuard code = unlines [ "#ifndef " ++ guardName
                                    , "#define " ++ guardName
                                    , ""
                                    , code
                                    , ""
                                    , "#endif // " ++ guardName
                                    ]

    guardName = map ((\c -> if c `elem` toBeChanged then '_' else c) . toUpper) hfile
      where
        toBeChanged = "./\\"


icompile :: (Compilable t internal) => t -> IO ()
icompile = icompileWith defaultOptions

icompileWith :: (Compilable t internal) => Options -> t -> IO ()
icompileWith opts = icompile' opts "test"

icompile' :: (Compilable t internal) => Options -> String -> t -> IO ()
icompile' opts functionName prg = do
    putStrLn "=============== Header ================"
    putStrLn $ sourceCode $ sctccrHeader compilationResult
    putStrLn "=============== Source ================"
    putStrLn $ sourceCode $ sctccrSource compilationResult
  where
    compilationResult = compileToCCore Interactive prg
                                       (NameExtractor.OriginalFunctionSignature functionName []) opts

icompileWithInfos :: (Compilable t internal) => t -> String -> Options -> SplitCompToCCoreResult
icompileWithInfos prg functionName = compileToCCore Interactive prg
                                                          (NameExtractor.OriginalFunctionSignature functionName [])

-- | Get the generated core for a program.
getCore prog = getCore' defaultOptions prog

-- | Print the generated core for a program.
printCore prog = print $ getCore' defaultOptions prog
