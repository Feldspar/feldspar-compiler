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

module Feldspar.NameExtractor where

import System.IO
import System.IO.Unsafe
import Language.Haskell.Exts
import Feldspar.Compiler.Error
import Feldspar.Compiler.Backend.C.Library

data OriginalFunctionSignature = OriginalFunctionSignature {
    originalFunctionName   :: String,
    originalParameterNames :: [Maybe String]
} deriving (Show, Eq)

nameExtractorError errorClass msg = handleError "NameExtractor" errorClass msg 

neutralName = "\\"++(r 4)++"/\\"++(r 7)++"\n )  ( ')"++(r 6)++"\n(  /  )"++(r 7)++"\n \\(__)|"
    where r n = replicate n ' '

ignore = OriginalFunctionSignature neutralName []

warning msg retval = unsafePerformIO $ do
    withColor Yellow $ putStrLn $ "Warning: " ++ msg
    return retval

-- Module SrcLoc ModuleName [OptionPragma] (Maybe WarningText) (Maybe [ExportSpec]) [ImportDecl] [Decl]
stripModule x = case x of
        Module a b c d e f g -> g

stripFunBind :: Decl -> OriginalFunctionSignature
stripFunBind x = case x of
        FunBind [Match a b c d e f] ->
            OriginalFunctionSignature (stripName b) (map stripPattern c) -- going for name and parameter list
            -- "Match SrcLoc Name [Pat] (Maybe Type) Rhs Binds"
        FunBind l@((Match a b c d e f):rest) | length l > 1 -> warning
            ("Ignoring function " ++ (stripName b) ++
            ": multi-pattern function definitions are not compilable as Feldspar functions.") ignore
        PatBind a b c d e -> case stripPattern b of
            Just functionName -> OriginalFunctionSignature functionName [] -- parameterless declarations (?)
            Nothing           -> nameExtractorError InternalError ("Unsupported pattern binding: " ++ show b)
        TypeSig a b c -> ignore --head b -- we don't need the type signature (yet)
        DataDecl a b c d e f g -> ignore
        InstDecl a b c d e -> ignore
        -- TypeDecl  SrcLoc Name [TyVarBind] Type
        TypeDecl a b c d -> ignore
        unknown -> nameExtractorError InternalError ("Unexpected language element [SFB/1]: " ++ show unknown
                                                ++ "\nPlease file a feature request with an example attached.")

stripPattern :: Pat -> Maybe String
stripPattern (PVar x)         = Just $ stripName x
stripPattern PWildCard        = Nothing
stripPattern (PAsPat x _)     = Just $ stripName x
stripPattern (PParen pattern) = stripPattern pattern
stripPattern _                = Nothing

stripName :: Name -> String
stripName (Ident a) = a
stripName (Symbol a) = a

stripModule2 (Module a b c d e f g) = b

stripModuleName (ModuleName x) = x

getModuleName :: FilePath -> String -> String -- filename, filecontents -> modulename
getModuleName fileName = stripModuleName . stripModule2 . fromParseResult . customizedParse fileName

usedExtensions = glasgowExts ++ [ExplicitForAll]

-- Ultimate debug function
getParseOutput fileName = parseFileWithMode (defaultParseMode { extensions = usedExtensions }) fileName

customizedParse fileName = parseFileContentsWithMode
  (defaultParseMode
    { extensions    = usedExtensions
    , parseFilename = fileName
    })

getFullDeclarationListWithParameterList :: FilePath -> String -> [OriginalFunctionSignature]
getFullDeclarationListWithParameterList fileName fileContents =
    map stripFunBind (stripModule $ fromParseResult $ customizedParse fileName fileContents )

functionNameNeeded :: String -> Bool
functionNameNeeded functionName = (functionName /= neutralName)

stripUnnecessary :: [String] -> [String]
stripUnnecessary = filter functionNameNeeded

printDeclarationList fileName = do
    handle <- openFile fileName ReadMode
    fileContents <- hGetContents handle
    return $ getDeclarationList fileContents

printDeclarationListWithParameterList fileName = do
    handle <- openFile fileName ReadMode
    fileContents <- hGetContents handle
    putStrLn $ show $ filter (functionNameNeeded . originalFunctionName) (getFullDeclarationListWithParameterList fileName fileContents)

printParameterListOfFunction :: FilePath -> String -> IO [Maybe String]
printParameterListOfFunction fileName functionName = getParameterList fileName functionName

-- The interface
getDeclarationList :: FilePath -> String -> [String] -- filename, filecontents -> Stringlist
getDeclarationList fileName = stripUnnecessary . (map originalFunctionName) . getFullDeclarationListWithParameterList fileName

getExtendedDeclarationList :: FilePath -> String -> [OriginalFunctionSignature] -- filename, filecontents -> ExtDeclList
getExtendedDeclarationList fileName fileContents =
  filter (functionNameNeeded . originalFunctionName)
    (getFullDeclarationListWithParameterList fileName fileContents)

getParameterListOld :: FilePath -> String -> String -> [Maybe String]
getParameterListOld fileName fileContents funName = originalParameterNames $ head $
  filter ((==funName) . originalFunctionName)
    (getExtendedDeclarationList fileName fileContents)

getParameterList :: FilePath -> String -> IO [Maybe String]
getParameterList fileName funName = do
    handle <- openFile fileName ReadMode
    fileContents <- hGetContents handle
    return $ originalParameterNames $ head $
        filter ((==funName) . originalFunctionName) (getExtendedDeclarationList fileName fileContents)
