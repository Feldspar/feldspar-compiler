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

{-# LANGUAGE CPP #-}
module Feldspar.Compiler.Frontend.CommandLine.API where

import qualified Feldspar.NameExtractor as NameExtractor
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Compiler
import Feldspar.Compiler.Imperative.FromCore
import Feldspar.Compiler.Frontend.CommandLine.API.Library
import Feldspar.Compiler.Backend.C.Library
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe
import qualified Data.Typeable as T
import Control.Monad

-- ====================================== System imports ==================================
import System.Directory
import System.FilePath
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import Control.Exception (catch, IOException)

data CompilationResult
    = CompilationSuccess
    | CompilationFailure
  deriving (Eq, Show, T.Typeable)

#ifdef RELEASE
releaseMode = True
#else
releaseMode = False
#endif

  -- A general interpreter body for interpreting an expression
generalInterpreterBody :: forall a . (T.Typeable (IO a))
                       => String -- the expression to interpret
                       -> Interpreter (IO a)
generalInterpreterBody expression = interpret expression (as::IO a)

-- A high-level interface for calling the interpreter
highLevelInterpreter :: T.Typeable (IO a)
                     => String -- the module name (for example My.Module)
                     -> String -- the input file name (for example "My/Module.hs")
                     -> [String] -- globalImportList
                     -> Bool -- need to import global modules qualified?
                     -> Interpreter (IO a) -- ^ an interpreter body
                     -> IO CompilationResult
highLevelInterpreter moduleName inputFileName importList needQualify interpreterBody = do
  actionToExecute <- runInterpreter $ do
    set [ languageExtensions := [GADTs, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving,
                                 DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
                                 FunctionalDependencies, ExistentialQuantification, Rank2Types, TypeOperators,
                                 EmptyDataDecls, GeneralizedNewtypeDeriving, TypeFamilies, ViewPatterns,
                                 UnknownExtension "ImplicitPrelude" ]
      ]
    unsafeSetGhcOption "-fcontext-stack=100"
    -- in relesae mode, globalImportList modules are package modules and shouldn't be loaded, only imported
    -- in normal mode, we need to load them before importing them
    loadModules $ inputFileName : if not releaseMode then importList else []
    setTopLevelModules [moduleName]
    -- Import modules qualified to prevent name collisions with user defined entities
    if needQualify
      then setImportsQ $ zip importList $ map Just importList
      else setImports importList
    interpreterBody
  case actionToExecute of
    Left err -> do
      printInterpreterError err
      return CompilationFailure
    Right action -> do
      action
      return CompilationSuccess
  -- either printInterpreterError id actionToExecute

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError (WontCompile []) = return ()
printInterpreterError (WontCompile (x:xs)) = do
    printGhcError x
    printInterpreterError (WontCompile xs)
  where
    printGhcError (GhcError {errMsg=s}) = hPutStrLn stderr s
printInterpreterError e = hPutStrLn stderr $ "Code generation failed: " ++ show e

handleOptions :: [OptDescr (a -> IO a)] -> a -> String -> IO (a, String)
handleOptions descriptors startOptions helpHeader = do
    args <- getArgs

    when (null args) (do
        putStrLn $ usageInfo helpHeader descriptors
        exitSuccess)

    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt Permute descriptors args

    when (length errors > 0) (do
        putStrLn $ concat errors
        putStrLn $ usageInfo helpHeader descriptors
        exitWith (ExitFailure 1))

    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions

    when (length nonOptions /= 1) (do
        putStrLn "ERROR: Exactly one input file expected."
        exitWith (ExitFailure 1))

    let inputFileName = replace (head nonOptions) "\\" "/" -- change it for multi-file operation

    return (opts, inputFileName)

removeFileIfPossible :: String -> IO ()
removeFileIfPossible filename = removeFile filename `Control.Exception.catch` (\(e :: IOException) -> return ())

prepareInputFile :: String -> IO ()
prepareInputFile inputFileName = do
    removeFileIfPossible $ replaceExtension inputFileName ".hi"
    removeFileIfPossible $ replaceExtension inputFileName ".o"

standaloneCompile :: (Compilable t internal) =>
    t -> FilePath -> FilePath -> NameExtractor.OriginalFunctionSignature -> Options -> IO ()
standaloneCompile prg inputFileName outputFileName originalFunctionSignature opts = do
    appendFile (makeCFileName outputFileName) $ sourceCode $ sctccrSource compilationResult
    appendFile (makeHFileName outputFileName) $ sourceCode $ sctccrHeader compilationResult
  where
    compilationResult = compileToCCore Standalone prg (Just outputFileName) IncludesNeeded originalFunctionSignature opts
