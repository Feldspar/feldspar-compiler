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

module Feldspar.Compiler.Backend.C.Plugin.TypeDefinitionGenerator where

import Data.List
import Feldspar.Transformation
import Feldspar.Compiler.Backend.C.CodeGeneration
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Error

-- ===========================================================================
--  == Type definition generator plugin
-- ===========================================================================

typeDefGenError :: ErrorClass -> String -> a
typeDefGenError = handleError "PluginArch/TypeDefinitionGenerator"

data TypeDefinitionGenerator = TypeDefinitionGenerator

getTypes :: Options -> Type -> [Entity ()]
getTypes options typ = {-trace ("DEBUG: "show typ) $-} case typ of
    StructType members -> concatMap (\(_,t) -> getTypes options t) members
                       ++ [StructDef {
                               structName      = toC options Declaration_pl (StructType members),
                               structMembers   = map (\(n,t) -> StructMember n t) members
                          }]
    ArrayType _ baseType -> getTypes options baseType
    _ -> []
    -- XXX complexType?

instance Transformation TypeDefinitionGenerator where
    type From TypeDefinitionGenerator = ()
    type To TypeDefinitionGenerator = ()
    type Down TypeDefinitionGenerator = Options
    type Up TypeDefinitionGenerator = ()
    type State TypeDefinitionGenerator = [Entity ()]

instance Transformable TypeDefinitionGenerator Module where
    transform selfpointer origState fromAbove origModule = defaultTransformationResult {
        result = (result defaultTransformationResult) {
            entities = nub (state defaultTransformationResult)
                     ++ entities (result defaultTransformationResult)
        }
    } where
        defaultTransformationResult = defaultTransform selfpointer origState fromAbove origModule

instance Transformable TypeDefinitionGenerator Variable where
    transform selfpointer origState fromAbove origVariable = defaultTransformationResult {
        state = state defaultTransformationResult ++ getTypes fromAbove (varType origVariable)
    } where
        defaultTransformationResult = defaultTransform selfpointer origState fromAbove origVariable

instance Plugin TypeDefinitionGenerator where
    type ExternalInfo TypeDefinitionGenerator = Options
    executePlugin TypeDefinitionGenerator externalInfo procedure = result
        $ transform TypeDefinitionGenerator [{-state-}] externalInfo procedure
