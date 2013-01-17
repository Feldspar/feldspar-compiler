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

{-# LANGUAGE ScopedTypeVariables #-}

module Main where
-- ====================================== Feldspar imports ==================================
import Feldspar.Compiler.Compiler
import qualified Feldspar.Compiler.Compiler as CompilerCore
import Feldspar.Compiler.Backend.C.Options
import qualified Feldspar.Compiler.Backend.C.Options as CoreOptions
import Feldspar.Compiler.Frontend.CommandLine.API.Options as StandaloneOptions
import Feldspar.Compiler.Frontend.CommandLine.API.Constants
import Feldspar.Compiler.Frontend.CommandLine.API.Library as StandaloneLib
import Feldspar.Compiler.Frontend.CommandLine.NameExtractor
import Feldspar.Compiler.Backend.C.Library
import Feldspar.Compiler.Frontend.CommandLine.API
import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Backend.C.CodeGeneration (compToCWithInfos)
import Feldspar.Compiler.Error
-- ====================================== System imports ==================================
import System.IO
import System.Exit
import System.Info
import System.Process
import System.IO.Error
import System.FilePath
import System.Directory
import System.Environment
import System.Console.GetOpt
-- ====================================== Control imports ==================================
import Control.Monad
import Control.Exception
import Control.Monad.Error
import Control.Monad.CatchIO
-- ====================================== Other imports ==================================
import Data.List
import Data.Maybe (fromMaybe)
import Data.Either (lefts, rights)
import Debug.Trace
import Language.Haskell.Interpreter


data CompilationError =
      InterpreterError InterpreterError
    | InternalErrorCall String

compileFunction :: String -> String -> CoreOptions.Options -> OriginalFunctionSignature
                -> Interpreter (Either (String, SplitModuleDescriptor) (String, CompilationError))
compileFunction inFileName outFileName coreOptions originalFunctionSignature = do
    let functionName = originalFunctionName originalFunctionSignature
    (SomeCompilable prg) <- interpret ("SomeCompilable " ++ functionName) (as::SomeCompilable)
    let splitModuleDescriptor = moduleSplitter $ executePluginChain originalFunctionSignature coreOptions prg
    -- XXX force evaluation in order to be able to catch the exceptions
    -- liftIO $ evaluate $ compToC coreOptions compilationUnit -- XXX somehow not enough(?!) -- counter-example: structexamples
    liftIO $ do
        tempdir <- Control.Exception.catch getTemporaryDirectory (\(_ :: IOException) -> return ".")
        (tempfile, temph) <- openTempFile tempdir "feldspar-temp.txt"
        let core = compileToCCore originalFunctionSignature coreOptions prg
        Control.Exception.finally (do hPutStrLn temph $ sourceCode $ sctccrSource core
                                      hPutStrLn temph $ sourceCode $ sctccrHeader core)
                                  (do hClose temph
                                      removeFileIfPossible tempfile)
        return $ Left (functionName, splitModuleDescriptor)

compileAllFunctions :: String -> String -> CoreOptions.Options -> [OriginalFunctionSignature]
                    -> Interpreter [Either (String, SplitModuleDescriptor) (String, CompilationError)]
compileAllFunctions inFileName outFileName options []     = return []
compileAllFunctions inFileName outFileName options (x:xs) = do
    let functionName = originalFunctionName x
    resultCurrent <- catchError (compileFunction inFileName outFileName options x)
                              (\(e::InterpreterError) -> return $ Right (functionName, InterpreterError e))
                          `Control.Monad.CatchIO.catch`
                          (\msg -> return $ Right (functionName, InternalErrorCall (errorPrefix ++ show (msg::Control.Exception.ErrorCall))))
    resultRest <- compileAllFunctions inFileName outFileName options xs
    return $ resultCurrent : resultRest

-- | Interpreter body for single-function compilation
singleFunctionCompilationBody :: String -> String -> CoreOptions.Options -> OriginalFunctionSignature
                              -> Interpreter (IO ())
singleFunctionCompilationBody inFileName outFileName coreOptions originalFunctionSignature = do
    liftIO $ fancyWrite $ "Compiling function " ++ originalFunctionName originalFunctionSignature ++ "..."
    SomeCompilable prg <-
        interpret ("SomeCompilable " ++ originalFunctionName originalFunctionSignature) (as::SomeCompilable)
    liftIO $ standaloneCompile inFileName outFileName originalFunctionSignature coreOptions prg
    return $ return ()

mergeModules :: [Module ()] -> Module ()
mergeModules [] = handleError "Standalone" InvariantViolation "Called mergeModules with an empty list"
mergeModules [x] = x
mergeModules l@(x:xs) = Module {
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
writeSummary (Left (functionName, x)) = do
    withColor Cyan $ putStr $ padFunctionName functionName
    withColor Green $ putStrLn "[OK]"
writeSummary (Right (functionName, msg)) = do
    withColor Cyan $ putStr $ padFunctionName functionName
    withColor Red $ putStrLn "[FAILED]"

-- | Interpreter body for multi-function compilation
multiFunctionCompilationBody :: String -> String -> CoreOptions.Options -> [OriginalFunctionSignature] -> Interpreter (IO ())
multiFunctionCompilationBody inFileName outFileName coreOptions declarationList = do
    let hIncludes = genIncludeLines coreOptions Nothing
    let cIncludes = genIncludeLines coreOptions (Just $ makeHFileName $ takeFileName outFileName)
    liftIO $ appendFile (makeHFileName outFileName) hIncludes
    liftIO $ appendFile (makeCFileName outFileName) cIncludes
    modules <- compileAllFunctions inFileName outFileName coreOptions declarationList
    liftIO $ do
        mapM_ writeError $ rights modules
        withColor Blue $ putStrLn "\n================= [ Summary of compilation results ] =================\n"
        mapM_ writeSummary modules
        let mergedCModules = mergeModules $ map (smdSource . snd) $ lefts modules
        let mergedHModules = mergeModules $ map (smdHeader . snd) $ lefts modules
        let cCompToCResult = compToCWithInfos coreOptions mergedCModules
        let hCompToCResult = compToCWithInfos coreOptions mergedHModules
        appendFile (makeCFileName outFileName) (sourceCode cCompToCResult) `Control.Exception.catch` errorHandler
        appendFile (makeHFileName outFileName) (sourceCode hCompToCResult) `Control.Exception.catch` errorHandler
        writeFile (makeDebugCFileName outFileName) (show $ debugModule cCompToCResult) `Control.Exception.catch` errorHandler
        writeFile (makeDebugHFileName outFileName) (show $ debugModule hCompToCResult) `Control.Exception.catch` errorHandler
    return $ return ()
    where
        errorHandler msg = withColor Red $ putStrLn $ errorPrefix ++ show (msg::Control.Exception.ErrorCall)

-- | Calculates the output file name.
convertOutputFileName :: String -> Maybe String -> String
convertOutputFileName inputFileName = fromMaybe (takeFileName $ dropExtension inputFileName)

makeBackup :: String -> IO ()
makeBackup filename = renameFile filename (filename ++ ".bak") `Control.Exception.catch` (\(_ :: IOException) -> return ())

main = do
    (opts, inputFileName) <- handleOptions optionDescriptors startOptions helpHeader
    let outputFileName = convertOutputFileName inputFileName (optOutputFileName opts)

    prepareInputFile inputFileName
    makeBackup $ makeHFileName outputFileName
    makeBackup $ makeCFileName outputFileName
    makeBackup $ makeDebugHFileName outputFileName
    makeBackup $ makeDebugCFileName outputFileName

    fileDescriptor <- openFile inputFileName ReadMode
    fileContents <- hGetContents fileDescriptor

    let mod = parse inputFileName fileContents
        declarationList = getExtendedDeclarationList mod
        moduleName = getModuleName mod
    fancyWrite $ "Compilation target: module " ++ moduleName
    fancyWrite $ "Output file: " ++ outputFileName

    let highLevelInterpreterWithModuleInfo =
            highLevelInterpreter moduleName inputFileName globalImportList False

    -- C code generation
    case optStandaloneMode opts of
        MultiFunction
          | null declarationList -> putStrLn "No functions to compile."
          | otherwise -> do
                fancyWrite $ "Number of functions to compile: " ++ show (length declarationList)
                highLevelInterpreterWithModuleInfo
                    (multiFunctionCompilationBody inputFileName outputFileName (optCompilerMode opts) declarationList)
                return ()
        SingleFunction funName -> do
            let originalFunctionSignatureNeeded =
                    case filter ((==funName).originalFunctionName) declarationList of
                            [a] -> a
                            []  -> error $ "Function " ++ funName ++ " not found"
                            _   -> error "Unexpected error SC/01"
            highLevelInterpreterWithModuleInfo
                (singleFunctionCompilationBody inputFileName outputFileName (optCompilerMode opts) originalFunctionSignatureNeeded)
            return ()
