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

module Feldspar.Compiler.Imperative.Plugin.ConstantFolding where

import Feldspar.Transformation

data ConstantFolding = ConstantFolding

instance Plugin ConstantFolding where
  type ExternalInfo ConstantFolding = ()
  executePlugin ConstantFolding _ procedure = result $ transform ConstantFolding () () procedure

instance Transformation ConstantFolding where
    type From ConstantFolding   = ()
    type To ConstantFolding     = ()
    type Down ConstantFolding   = ()
    type Up ConstantFolding     = ()
    type State ConstantFolding  = ()

instance Transformable ConstantFolding Expression where
    transform t s d f@FunctionCall{} = case funMode $ function f' of
        Infix -> case funName $ function f' of
            "+"     -> tr' $ elimParamIf (isConstIntN 0) True  $ result tr
            "-"     -> tr' $ elimParamIf (isConstIntN 0) False $ result tr
            "*"     -> tr' $ elimParamIf (isConstIntN 1) True  $ result tr
            _       -> tr
        _       -> tr
        where
            tr = defaultTransform t s d f
            tr' x = tr {result = x}
            f' = result tr
            isConstIntN n (ConstExpr (IntConst i _)) = n == i
            isConstIntN _ _ = False

            elimParamIf predicate flippable funCall@(FunctionCall (Function _ _ Infix) (x:xs))
                | predicate (head xs)      = x
                | flippable && predicate x = head xs
                | otherwise                = funCall
            elimParamIf _ _ funCall        = funCall
    transform t s d e = defaultTransform t s d e
