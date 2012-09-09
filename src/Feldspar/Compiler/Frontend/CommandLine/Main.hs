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

module Main where
-- ====================================== Feldspar imports ==================================
import Feldspar.NameExtractor
import Feldspar.Compiler.Compiler
import qualified Feldspar.Compiler.Compiler as CompilerCore
import Feldspar.Compiler.Backend.C.Options
import qualified Feldspar.Compiler.Backend.C.Options as CoreOptions
import Feldspar.Compiler.Frontend.CommandLine.API.Options as StandaloneOptions
import Feldspar.Compiler.Frontend.CommandLine.API.Constants
import Feldspar.Compiler.Frontend.CommandLine.API.Library as StandaloneLib
import Feldspar.Compiler.Backend.C.Library
import Feldspar.Compiler.Frontend.CommandLine.API
import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Backend.C.CodeGeneration
import Feldspar.Compiler.Error
import Feldspar.Compiler.Backend.C.Plugin.PrettyPrint
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
import Debug.Trace
import Language.Haskell.Interpreter


data CompilationError =
      InterpreterError InterpreterError
    | InternalErrorCall String

compileFunction :: String -> String -> CoreOptions.Options -> OriginalFunctionSignature
                -> Interpreter (String, Either SplitModuleDescriptor CompilationError)
compileFunction inFileName outFileName coreOptions originalFunctionSignature = do
    let functionName = originalFunctionName originalFunctionSignature
    (SomeCompilable prg) <- interpret ("SomeCompilable " ++ functionName) (as::SomeCompilable)
    let splitModuleDescriptor = executePluginChain Standalone prg originalFunctionSignature coreOptions
    -- XXX force evaluation in order to be able to catch the exceptions
    -- liftIO $ evaluate $ compToC coreOptions compilationUnit -- XXX somehow not enough(?!) -- counter-example: structexamples
    result <- liftIO $ do
        tempdir <- System.IO.Error.catch (getTemporaryDirectory) (\_ -> return ".")
        (tempfile, temph) <- openTempFile tempdir "feldspar-temp.txt"
        let core = compileToCCore Standalone prg (Just outFileName) IncludesNeeded originalFunctionSignature coreOptions
        Control.Exception.finally (do hPutStrLn temph $ sourceCode $ sctccrSource core
                                      hPutStrLn temph $ sourceCode $ sctccrHeader core)
                                  (do hClose temph
                                      removeFileIfPossible tempfile)
        return $ (functionName, Left splitModuleDescriptor)
    return result

compileAllFunctions :: String -> String -> CoreOptions.Options -> [OriginalFunctionSignature]
                    -> Interpreter [(String, Either SplitModuleDescriptor CompilationError)]
compileAllFunctions inFileName outFileName options [] = return []
compileAllFunctions inFileName outFileName options (x:xs) = do
    let functionName = originalFunctionName x
    resultCurrent <- (catchError (compileFunction inFileName outFileName options x)
                              (\(e::InterpreterError) -> return $ (functionName, Right $ InterpreterError e)))
                          `Control.Monad.CatchIO.catch`
                          (\msg -> return $ (functionName,
                                             Right $ InternalErrorCall $ errorPrefix ++ show (msg::Control.Exception.ErrorCall)))
    resultRest <- compileAllFunctions inFileName outFileName options xs
    return $ resultCurrent : resultRest

-- | Interpreter body for single-function compilation
singleFunctionCompilationBody :: String -> String -> CoreOptions.Options -> OriginalFunctionSignature
                              -> Interpreter (IO ())
singleFunctionCompilationBody inFileName outFileName coreOptions originalFunctionSignature = do
    liftIO $ fancyWrite $ "Compiling function " ++ (originalFunctionName originalFunctionSignature) ++ "..."
    (SomeCompilable prg) <-
        interpret ("SomeCompilable " ++ originalFunctionName originalFunctionSignature) (as::SomeCompilable)
    liftIO $ standaloneCompile prg inFileName outFileName originalFunctionSignature coreOptions
    return $ return ()

mergeModules :: [Module ()] -> Module ()
mergeModules [] = handleError "Standalone" InvariantViolation "Called mergeModules with an empty list"
mergeModules [x] = x
mergeModules l@(x:xs) = Module {
    entities = nub $ entities x ++ (entities $ mergeModules xs), -- nub is in fact a "global plugin" here
    moduleLabel = ()
}

padFunctionName :: String -> String
padFunctionName n = StandaloneLib.rpadWith 50 '.' $ "Function " ++ n

writeErrors :: (String, Either a CompilationError) -> IO ()
writeErrors (functionName, Left x) = return ()
writeErrors (functionName, Right err) = case err of 
    InterpreterError ie -> do
        withColor Red $ putStrLn $ "Error in function " ++ functionName ++ ":"
        printInterpreterError ie
    InternalErrorCall ec -> do
        withColor Red $ putStrLn $ "Error in function " ++ functionName ++ ":"
        withColor Red $ putStrLn ec

writeSummary :: (String, Either a CompilationError) -> IO ()
writeSummary (functionName, Left x) = do
    withColor Cyan $ putStr $ padFunctionName functionName
    withColor Green $ putStrLn "[OK]"
writeSummary (functionName, Right msg) = do
    withColor Cyan $ putStr $ padFunctionName functionName
    withColor Red $ putStrLn "[FAILED]"

filterLefts :: [(String, Either a b)] -> [a]
filterLefts [] = []
filterLefts [(_,Left x)]  = [x]
filterLefts [(_,Right _)] = []
filterLefts ((_,Left x):xs)  = x : filterLefts xs
filterLefts ((_,Right _):xs) = filterLefts xs

-- | Interpreter body for multi-function compilation
multiFunctionCompilationBody :: String -> String -> CoreOptions.Options -> [OriginalFunctionSignature] -> Interpreter (IO ())
multiFunctionCompilationBody inFileName outFileName coreOptions declarationList = do
    let (hIncludes, hLineNum) = genIncludeLines coreOptions Nothing
    let (cIncludes, cLineNum) = genIncludeLines coreOptions (Just outFileName)
    liftIO $ appendFile (makeHFileName outFileName) $ hIncludes
    liftIO $ appendFile (makeCFileName outFileName) $ cIncludes
    modules <- compileAllFunctions inFileName outFileName coreOptions declarationList
    liftIO $ do
        mapM writeErrors modules
        withColor Blue $ putStrLn "\n================= [ Summary of compilation results ] =================\n"
        mapM writeSummary modules
        let mergedCModules = mergeModules $ map smdSource $ filterLefts modules
        let mergedHModules = mergeModules $ map smdHeader $ filterLefts modules
        let cCompToCResult = compToCWithInfos ((coreOptions, Declaration_pl), cLineNum) mergedCModules
        let hCompToCResult = compToCWithInfos ((coreOptions, Declaration_pl), hLineNum) mergedHModules
        (appendFile (makeCFileName outFileName) $ fst $ snd cCompToCResult) `Control.Exception.catch` errorHandler
        (appendFile (makeHFileName outFileName) $ fst $ snd hCompToCResult) `Control.Exception.catch` errorHandler
        (writeFile (makeDebugCFileName outFileName) $ show $ fst cCompToCResult) `Control.Exception.catch` errorHandler
        (writeFile (makeDebugHFileName outFileName) $ show $ fst hCompToCResult) `Control.Exception.catch` errorHandler
    return $ return ()
    where
        errorHandler = (\msg -> withColor Red $ putStrLn $ errorPrefix ++ show (msg::Control.Exception.ErrorCall))

-- | Calculates the output file name.
convertOutputFileName :: String -> Maybe String -> String
convertOutputFileName inputFileName maybeOutputFileName = case maybeOutputFileName of
    Nothing -> takeFileName $ dropExtension inputFileName -- remove takeFileName to return the full path
    Just overriddenFileName -> overriddenFileName

makeBackup :: String -> IO ()
makeBackup filename = renameFile filename (filename ++ ".bak") `Prelude.catch` (const $ return())

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

    let declarationList = getExtendedDeclarationList inputFileName fileContents
    let moduleName = getModuleName inputFileName fileContents
    fancyWrite $ "Compilation target: module " ++ moduleName
    fancyWrite $ "Output file: " ++ outputFileName

    let highLevelInterpreterWithModuleInfo body = 
            highLevelInterpreter moduleName inputFileName globalImportList False body

    -- C code generation
    case optStandaloneMode opts of
        MultiFunction 
          | length declarationList == 0 -> putStrLn "No functions to compile."
          | otherwise -> do
                fancyWrite $ "Number of functions to compile: " ++ (show $ length declarationList)
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
