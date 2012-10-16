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

module Feldspar.Compiler.Backend.C.Plugin.VariableRoleAssigner where

import Feldspar.Transformation

data VariableRoleAssigner = VariableRoleAssigner

data Parameters = Parameters
    { inParametersVRA  :: [String]
    , outParametersVRA :: [String]
    }

instance Transformation VariableRoleAssigner where
    type From VariableRoleAssigner = ()
    type To VariableRoleAssigner = ()
    type Down VariableRoleAssigner = Parameters
    type Up VariableRoleAssigner = ()
    type State VariableRoleAssigner = ()

instance Transformable VariableRoleAssigner Variable where
        transform _ _ d v = Result v' () () where
            v' = v { varRole = if (varName v `elem` outParametersVRA d) ||
                            (isComposite v && (varName v `elem` inParametersVRA d))
                        then Pointer else Value
                   , varLabel = ()
                   }

instance Transformable VariableRoleAssigner Entity where
        transform t s _ p@(ProcDef _ i o _ _ _) = defaultTransform t s d' p where
            d' = Parameters
                    { inParametersVRA = map varName i
                    , outParametersVRA = map varName o
                    }
        transform t s d p = defaultTransform t s d p

instance Plugin VariableRoleAssigner where
    type ExternalInfo VariableRoleAssigner = ()
    executePlugin self@VariableRoleAssigner _ procedure = 
        result $ transform self ({-state-}) (Parameters [] []) procedure

isComposite :: Variable () -> Bool
isComposite v = case varType v of
    ArrayType{}  -> True
    StructType{} -> True
    _            -> False

