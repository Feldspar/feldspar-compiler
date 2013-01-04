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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.Plugin.Naming where

import Feldspar.Transformation

import qualified Feldspar.NameExtractor as Precompiler
import Feldspar.Compiler.Error
import Feldspar.Compiler.Backend.C.Library

-- ===========================================================================
--  == Precompilation plugin
-- ===========================================================================

data SignatureInformation = SignatureInformation {
    originalFunctionName              :: String,
    generatedImperativeParameterNames :: [String],
    originalParameterNames            :: Maybe [Maybe String]
} deriving (Show, Eq)

instance Default SignatureInformation where def = precompilationError InternalError "Default value should not be used"

precompilationError :: ErrorClass -> String -> a
precompilationError = handleError "PluginArch/Naming"

data Precompilation = Precompilation

instance Transformation Precompilation where
    type From Precompilation = ()
    type To Precompilation = ()
    type Down Precompilation = SignatureInformation
    type Up Precompilation = ()
    type State Precompilation = ()


instance Transformable Precompilation Entity where
        transform t s d x@(ProcDef n KMain i _ _ _ _)
            = tr { result = (result tr){ procName = n' } }
          where
            d' = d { generatedImperativeParameterNames = map varName i }
            tr = defaultTransform t s d' x
            n' = originalFunctionName d
        transform t s d x@(ProcDef n k _ _ _ _ _)
            | k `elem` procedureKindsToPrefix = tr { result = (result tr){ procName = n' } }
          where
            n' = prefix d n
            tr = defaultTransform t s d' x
            d' = d{ generatedImperativeParameterNames = [] }
        transform t s d x@ProcDef{} = defaultTransform t s d' x
          where
            d' = d{ generatedImperativeParameterNames = [] }
        transform t s d x = defaultTransform t s d x


instance Transformable Precompilation Variable where
    transform _ s d v = Result newVar s def
      where
        newVar = v 
            { varName = maybeStr2Str (getVariableName d $ varName v) ++ varName v
            , varLabel = ()
            }

instance Transformable Precompilation ActualParameter where
    transform _ s d (FunParameter n k addr _)
        | k `elem` procedureKindsToPrefix
            = Result (FunParameter (prefix d n) k addr ()) s def
    transform t s d x = defaultTransform t s d x

instance Transformable Precompilation Program where
    transform t s d c@(ProcedureCall n k _ _ _)
        | k `elem` procedureKindsToPrefix = tr { result = (result tr){ procCallName = n' } }
      where
        tr = defaultTransform t s d c
        n' = prefix d n
    transform t s d x = defaultTransform t s d x

procedureKindsToPrefix :: [Kind]
procedureKindsToPrefix = [KNoInline, KTask]

prefix :: SignatureInformation -> String -> String
prefix d n = originalFunctionName d ++ "_" ++ n

getVariableName :: SignatureInformation -> String -> Maybe String
getVariableName siginf origname
  | Just originalParameterNameList <- originalParameterNames siginf
  , length (generatedImperativeParameterNames siginf) == length originalParameterNameList
  , (r:_) <- filter ((origname ==).fst) (zip (generatedImperativeParameterNames siginf) originalParameterNameList)
  = snd r
  | otherwise = Nothing

maybeStr2Str :: Maybe String -> String
maybeStr2Str (Just s) = s ++ "_"
maybeStr2Str Nothing = ""

data PrecompilationExternalInfo = PrecompilationExternalInfo {
    originalFunctionSignature :: Precompiler.OriginalFunctionSignature, 
    inputParametersDescriptor :: [Int],
    compilationMode :: CompilationMode
}

inflate :: Int -> [Maybe String] -> [Maybe String]
inflate target list | length list <  target = inflate target (list++[Nothing])
                    | length list == target = list
                    | otherwise             = precompilationError InternalError "Unexpected situation in 'inflate'"

-- Replicates each element of the [parameter list given by the precompiler] based on the input parameter descriptor
parameterNameListConsolidator :: PrecompilationExternalInfo -> [Maybe String]
parameterNameListConsolidator eInf
  = concatMap (uncurry replicate)
      (zip (inputParametersDescriptor eInf)
        (Precompiler.originalParameterNames $ originalFunctionSignature eInf))

instance Plugin Precompilation where
    type ExternalInfo Precompilation = PrecompilationExternalInfo
    executePlugin Precompilation externalInfo procedure = result
        $ transform Precompilation ({-state-}) SignatureInformation{
            originalFunctionName = Precompiler.originalFunctionName $ originalFunctionSignature externalInfo,
            generatedImperativeParameterNames = precompilationError InternalError "GIPN should have been overwritten", 
            originalParameterNames = case compilationMode externalInfo of
                Standalone -> Just $ parameterNameListConsolidator externalInfo
                Interactive -> Nothing -- no parameter name handling in interactive mode
         } procedure

