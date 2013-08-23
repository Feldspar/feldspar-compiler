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

module Feldspar.Compiler.Frontend.CommandLine.NameExtractor (getModuleInfo) where

import Data.Maybe (mapMaybe)
import System.IO
import System.IO.Unsafe
import System.Console.ANSI
import Language.Haskell.Exts hiding (parse)
import Feldspar.Compiler.Error
import Feldspar.Compiler.Frontend.CommandLine.API.Library

nameExtractorError :: ErrorClass -> String -> a
nameExtractorError = handleError "NameExtractor"

ignore = Nothing

warning :: String -> a -> a
warning msg retval = unsafePerformIO $ do
    withColor Yellow $ putStrLn $ "Warning: " ++ msg
    return retval

-- Module SrcLoc ModuleName [OptionPragma] (Maybe WarningText) (Maybe [ExportSpec]) [ImportDecl] [Decl]
declarations :: Module -> [Decl]
declarations (Module _ _ _ _ _ _ g) = g

stripFunBind :: Decl -> Maybe String
stripFunBind (FunBind [Match _ b c _ _ _])
  = Just $ stripName b
            -- "Match SrcLoc Name [Pat] (Maybe Type) Rhs Binds"
stripFunBind (FunBind l@(Match _ b _ _ _ _ : tl)) | not (null tl) = warning
            ("Ignoring function " ++ stripName b ++
            ": multi-pattern function definitions are not compilable as Feldspar functions.") ignore
stripFunBind (PatBind _ b _ _ _) = case stripPattern b of
            Just functionName -> Just $ functionName -- parameterless declarations (?)
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
stripPattern (PAsPat x _)     = Just $ stripName x
stripPattern (PParen pattern) = stripPattern pattern
stripPattern _                = Nothing

stripName :: Name -> String
stripName (Ident a) = a
stripName (Symbol a) = a

moduleName :: Module -> String
moduleName (Module _ (ModuleName n) _ _ _ _ _) = n

parse :: FilePath -> String -> Module
parse fileName contents = fromParseResult $ parseFileContentsWithMode
  defaultParseMode
    { extensions    = glasgowExts ++ [ExplicitForAll]
    , parseFilename = fileName
    }
  contents

getExtendedDeclarationList :: Module -> [String]
getExtendedDeclarationList mod = mapMaybe stripFunBind (declarations mod)

getModuleInfo :: FilePath -> String -> (String, [String])
getModuleInfo fileName contents = (moduleName mod, getExtendedDeclarationList mod)
  where mod = parse fileName contents
