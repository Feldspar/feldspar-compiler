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

import Data.Maybe (catMaybes)
import System.IO
import System.IO.Unsafe
import Language.Haskell.Exts
import Feldspar.Compiler.Error
import Feldspar.Compiler.Backend.C.Library

data OriginalFunctionSignature = OriginalFunctionSignature {
    originalFunctionName   :: String,
    originalParameterNames :: [Maybe String]
} deriving (Show, Eq)

nameExtractorError :: ErrorClass -> String -> a
nameExtractorError = handleError "NameExtractor"

neutralName :: String
neutralName = "\\"++ r 4 ++"/\\"++ r 7 ++"\n )  ( ')"++ r 6 ++"\n(  /  )"++ r 7 ++"\n \\(__)|"
    where r n = replicate n ' '

ignore = Nothing

warning :: String -> a -> a
warning msg retval = unsafePerformIO $ do
    withColor Yellow $ putStrLn $ "Warning: " ++ msg
    return retval

-- Module SrcLoc ModuleName [OptionPragma] (Maybe WarningText) (Maybe [ExportSpec]) [ImportDecl] [Decl]
declarations :: Module -> [Decl]
declarations (Module _ _ _ _ _ _ g) = g

stripFunBind :: Decl -> Maybe OriginalFunctionSignature
stripFunBind (FunBind [Match _ b c _ _ _])
  = Just $ OriginalFunctionSignature (stripName b) (map stripPattern c) -- going for name and parameter list
            -- "Match SrcLoc Name [Pat] (Maybe Type) Rhs Binds"
stripFunBind (FunBind l@(Match _ b _ _ _ _ : tl)) | not (null tl) = warning
            ("Ignoring function " ++ stripName b ++
            ": multi-pattern function definitions are not compilable as Feldspar functions.") ignore
stripFunBind (PatBind _ b _ _ _) = case stripPattern b of
            Just functionName -> Just $ OriginalFunctionSignature functionName [] -- parameterless declarations (?)
            Nothing           -> nameExtractorError InternalError ("Unsupported pattern binding: " ++ show b)
stripFunBind TypeSig{} = ignore -- we don't need the type signature (yet)
stripFunBind DataDecl{} = ignore
stripFunBind InstDecl{} = ignore
        -- TypeDecl  SrcLoc Name [TyVarBind] Type
stripFunBind TypeDecl{} = ignore
stripFunBind unknown = nameExtractorError InternalError ("Unexpected language element [SFB/1]: " ++ show unknown
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

stripModule2 :: Module -> ModuleName
stripModule2 (Module _ b _ _ _ _ _) = b

stripModuleName :: ModuleName -> String
stripModuleName (ModuleName x) = x

getModuleName :: FilePath -> String -> String -- filename, filecontents -> modulename
getModuleName fileName = stripModuleName . stripModule2 . fromParseResult . customizedParse fileName

usedExtensions :: [Extension]
usedExtensions = glasgowExts ++ [ExplicitForAll]

-- Ultimate debug function
getParseOutput :: FilePath -> IO (ParseResult Module)
getParseOutput = parseFileWithMode (defaultParseMode { extensions = usedExtensions })

customizedParse :: FilePath -> FilePath -> ParseResult Module
customizedParse fileName = parseFileContentsWithMode
  (defaultParseMode
    { extensions    = usedExtensions
    , parseFilename = fileName
    })

printParameterListOfFunction :: FilePath -> String -> IO [Maybe String]
printParameterListOfFunction = getParameterList

getExtendedDeclarationList :: FilePath -> String -> [OriginalFunctionSignature] -- filename, filecontents -> ExtDeclList
getExtendedDeclarationList fileName fileContents
  = catMaybes $ map stripFunBind (declarations $ fromParseResult $ customizedParse fileName fileContents)  

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
