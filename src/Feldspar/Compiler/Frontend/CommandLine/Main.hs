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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
-- ====================================== Feldspar imports ==================================
import Feldspar.Core.Constructs (SyntacticFeld)
import Feldspar.Compiler.Compiler
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Frontend.CommandLine.API.Options as StandaloneOptions
import Feldspar.Compiler.Frontend.CommandLine.API.Constants
import Feldspar.Compiler.Frontend.CommandLine.API.Library as StandaloneLib
import Feldspar.Compiler.Frontend.CommandLine.NameExtractor (getModuleInfo)
import Feldspar.Compiler.Backend.C.Library
import Feldspar.Compiler.Frontend.CommandLine.API
import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Error
-- ====================================== System imports ==================================
import System.IO
import System.FilePath
import System.Directory
import System.Console.ANSI
-- ====================================== Control imports ==================================
import Control.Exception
import Control.Monad.Error
import Control.Monad.CatchIO
-- ====================================== Other imports ==================================
import Data.List
import Data.Maybe (fromMaybe)
import Data.Either (lefts, rights)
import Data.Typeable (Typeable(..))
import Language.Haskell.Interpreter

data SomeCompilable = forall a . SyntacticFeld a => SomeCompilable a
    deriving (Typeable)

data CompilationError =
      InterpreterError InterpreterError
    | InternalErrorCall String

compileFunction :: String -> String -> Options -> String
                -> Interpreter (Either (String, SplitModuleDescriptor) (String, CompilationError))
compileFunction _ _ coreOptions functionName = do
    (SomeCompilable prg) <- interpret ("SomeCompilable " ++ functionName) (as::SomeCompilable)
    let splitModuleDescriptor = moduleSplitter $ executePluginChain functionName coreOptions prg
    -- XXX force evaluation in order to be able to catch the exceptions
    -- liftIO $ evaluate $ compToC coreOptions compilationUnit -- XXX somehow not enough(?!) -- counter-example: structexamples
    liftIO $ do
        tempdir <- Control.Exception.catch getTemporaryDirectory (\(_ :: IOException) -> return ".")
        (tempfile, temph) <- openTempFile tempdir "feldspar-temp.txt"
        let core = compileToCCore functionName coreOptions prg
        Control.Exception.finally (do hPutStrLn temph $ sourceCode $ sctccrSource core
                                      hPutStrLn temph $ sourceCode $ sctccrHeader core)
                                  (do hClose temph
                                      removeFileIfPossible tempfile)
        return $ Left (functionName, splitModuleDescriptor)

compileAllFunctions :: String -> String -> Options -> [String]
                    -> Interpreter [Either (String, SplitModuleDescriptor) (String, CompilationError)]
compileAllFunctions inFileName outFileName options = mapM go
  where
    go functionName = do
        catchError (compileFunction inFileName outFileName options functionName)
                 (\(e::InterpreterError) -> return $ Right (functionName, InterpreterError e))
             `Control.Monad.CatchIO.catch`
             (\msg -> return $ Right (functionName, InternalErrorCall (errorPrefix ++ show (msg::Control.Exception.ErrorCall))))

-- | Interpreter body for single-function compilation
singleFunctionCompilationBody :: String -> String -> Options -> String
                              -> Interpreter (IO ())
singleFunctionCompilationBody inFileName outFileName coreOptions functionName = do
    liftIO $ fancyWrite $ "Compiling function " ++ functionName ++ "..."
    SomeCompilable prg <-
        interpret ("SomeCompilable " ++ functionName) (as::SomeCompilable)
    liftIO $ standaloneCompile inFileName outFileName functionName coreOptions prg
    return $ return ()

mergeModules :: [Module ()] -> Module ()
mergeModules [] = handleError "Standalone" InvariantViolation "Called mergeModules with an empty list"
mergeModules [x] = x
mergeModules (x:xs) = Module {
    entities = nub $ entities x ++ entities (mergeModules xs) -- nub is in fact a "global plugin" here
}

padFunctionName :: String -> String
padFunctionName n = StandaloneLib.rpadWith 50 '.' $ "Function " ++ n

writeError :: (String, CompilationError) -> IO ()
writeError (functionName, InterpreterError ie) = do
    withColor Red $ putStrLn $ "Error in function " ++ functionName ++ ":"
    printInterpreterError ie
writeError (functionName, InternalErrorCall ec) = do
    withColor Red $ putStrLn $ "Error in function " ++ functionName ++ ":"
    withColor Red $ putStrLn ec

writeSummary :: Either (String, a) (String, CompilationError) -> IO ()
writeSummary (Left (functionName, _)) = do
    withColor Cyan $ putStr $ padFunctionName functionName
    withColor Green $ putStrLn "[OK]"
writeSummary (Right (functionName, _)) = do
    withColor Cyan $ putStr $ padFunctionName functionName
    withColor Red $ putStrLn "[FAILED]"

-- | Interpreter body for multi-function compilation
multiFunctionCompilationBody :: String -> String -> Options -> [String] -> Interpreter (IO ())
multiFunctionCompilationBody inFileName outFileName coreOptions declarationList = do
    let hOutFileName   = makeHFileName outFileName
        cOutFileName   = makeCFileName outFileName
        hdbgOutFileName = makeDebugFileName hOutFileName
        cdbgOutFileName = makeDebugFileName cOutFileName
    let hIncludes = genIncludeLines coreOptions Nothing
    let cIncludes = genIncludeLines coreOptions (Just $ takeFileName hOutFileName)
    liftIO $ appendFile hOutFileName hIncludes
    liftIO $ appendFile cOutFileName cIncludes
    modules <- compileAllFunctions inFileName outFileName coreOptions declarationList
    liftIO $ do
        mapM_ writeError $ rights modules
        withColor Blue $ putStrLn "\n================= [ Summary of compilation results ] =================\n"
        mapM_ writeSummary modules
        let mergedCModules = mergeModules $ map (smdSource . snd) $ lefts modules
        let mergedHModules = mergeModules $ map (smdHeader . snd) $ lefts modules
        let compToCResult = moduleToCCore coreOptions (mergedHModules, mergedCModules)
        let cCompToCResult = smdSource compToCResult
            hCompToCResult = smdHeader compToCResult
        appendFile cOutFileName (sourceCode cCompToCResult) `Control.Exception.catch` errorHandler
        appendFile hOutFileName (sourceCode hCompToCResult) `Control.Exception.catch` errorHandler
        writeFile cdbgOutFileName (show $ debugModule cCompToCResult) `Control.Exception.catch` errorHandler
        writeFile hdbgOutFileName (show $ debugModule hCompToCResult) `Control.Exception.catch` errorHandler
    return $ return ()
    where
        errorHandler msg = withColor Red $ putStrLn $ errorPrefix ++ show (msg::Control.Exception.ErrorCall)

-- | Calculates the output file name.
convertOutputFileName :: String -> Maybe String -> String
convertOutputFileName inputFileName = fromMaybe (takeFileName $ dropExtension inputFileName)

makeBackup :: String -> IO ()
makeBackup filename = renameFile filename (filename ++ ".bak") `Control.Exception.catch` (\(_ :: IOException) -> return ())

main :: IO ()
main = do
    (opts, inputFileName) <- handleOptions optionDescriptors startOptions helpHeader
    let outputFileName = convertOutputFileName inputFileName (optOutputFileName opts)
        cOutputFileName = makeCFileName outputFileName
        hOutputFileName = makeHFileName outputFileName

    prepareInputFile inputFileName
    makeBackup cOutputFileName
    makeBackup hOutputFileName
    makeBackup $ makeDebugFileName hOutputFileName
    makeBackup $ makeDebugFileName cOutputFileName

    fileDescriptor <- openFile inputFileName ReadMode
    fileContents <- hGetContents fileDescriptor

    let (moduleName, declarationList) = getModuleInfo inputFileName fileContents
    fancyWrite $ "Compilation target: module " ++ moduleName
    fancyWrite $ "Output file: " ++ outputFileName

    let highLevelInterpreterWithModuleInfo =
            highLevelInterpreter moduleName inputFileName globalImportList

    -- C code generation
    case optStandaloneMode opts of
        MultiFunction
          | null declarationList -> putStrLn "No functions to compile."
          | otherwise -> do
                fancyWrite $ "Number of functions to compile: " ++ show (length declarationList)
                _ <- highLevelInterpreterWithModuleInfo
                    (multiFunctionCompilationBody inputFileName outputFileName (optCompilerMode opts) declarationList)
                return ()
        SingleFunction functionName -> do
            let originalFunctionSignatureNeeded
                  | functionName `elem` declarationList = functionName
                  | otherwise = error $ "Function " ++ functionName ++ " not found"
            _ <- highLevelInterpreterWithModuleInfo
                (singleFunctionCompilationBody inputFileName outputFileName (optCompilerMode opts) originalFunctionSignatureNeeded)
            return ()
