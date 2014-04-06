{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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

import Feldspar.Core.Constructs (SyntacticFeld)
import Feldspar.Compiler.Compiler
import Feldspar.Compiler.Imperative.FromCore
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Backend.C.Library
import Feldspar.Compiler.Imperative.Representation (Module(..))

import Data.Char
import Control.Monad (when)
import System.FilePath (takeFileName)

-- ================================================================================================
--  == Interactive compilation
-- ================================================================================================

compile :: (SyntacticFeld t) => t -> FilePath -> String -> Options -> IO ()
compile prg fileName funName opts = writeFiles compRes fileName funName opts
  where compRes = compileToCCore funName opts prg

writeFiles :: SplitModule -> FilePath -> String -> Options -> IO ()
writeFiles prg fileName functionName opts = do
    writeFile cfile $ unlines [ "#include \"" ++ takeFileName hfile ++ "\""
                              , "\n"
                              , sourceCode $ implementation prg
                              ]
    writeFile hfile $ withIncludeGuard $ sourceCode $ interface prg
  where
    hfile = makeHFileName fileName
    cfile = makeCFileName fileName

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

icompile :: (SyntacticFeld t) => t -> IO ()
icompile = icompileWith defaultOptions

icompileWith :: (SyntacticFeld t) => Options -> t -> IO ()
icompileWith opts = icompile' opts "test"

icompile' :: (SyntacticFeld t) => Options -> String -> t -> IO ()
icompile' opts functionName prg = do
    let res = compileToCCore functionName opts prg
    when (printHeader opts) $ do
      putStrLn "=============== Header ================"
      putStrLn $ sourceCode $ interface res
      putStrLn "=============== Source ================"
    putStrLn $ sourceCode $ implementation res

-- | Get the generated core for a program.
getCore :: (SyntacticFeld t) => t -> Module ()
getCore = getCore' defaultOptions

-- | Print the generated core for a program.
printCore :: (SyntacticFeld t) => t -> IO ()
printCore prog = print $ getCore' defaultOptions prog
