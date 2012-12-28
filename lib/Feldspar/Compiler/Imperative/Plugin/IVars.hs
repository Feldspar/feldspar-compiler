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
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.Plugin.IVars where

import Feldspar.Transformation

import Data.List (isPrefixOf)

data IVarPlugin = IVarPlugin

instance Transformation IVarPlugin
  where
    type From IVarPlugin     = ()
    type To IVarPlugin       = ()
    type Down IVarPlugin     = Bool -- True if the code is in a task.
    type Up IVarPlugin       = ()
    type State IVarPlugin    = ()

instance Plugin IVarPlugin
  where
    type ExternalInfo IVarPlugin = ()
    executePlugin _ _ = result . transform IVarPlugin () False

instance Transformable IVarPlugin Entity where
    transform t _ _ p@ProcDef{} = defaultTransform t () (isTask p) p
      where
        isTask proc = "task" `isPrefixOf` procName proc    -- TODO: this is hacky :)
    transform t _ d p = defaultTransform t () d p

instance Transformable IVarPlugin Program where
    transform _ _ d (ProcedureCall name k ps _ _)
        | "ivar_get" `isPrefixOf` name
        = Result pc' () ()
      where
        pc' = ProcedureCall name' k ps () ()
        name' | d           = name
              | otherwise   = name ++ "_nontask"
    transform t _ d x = defaultTransform t () d x

instance Transformable IVarPlugin Block where
    transform t _ d b = Result b{ blockBody = body' } () ()
      where
        body' = Sequence prg () ()
        prg = result (transform t () d $ blockBody b) : destrs
        iVars = filter isIVar $ map declVar $ locals b
        isIVar v = case varType v of
            IVarType _  -> True
            _           -> False
        ivarFun s v = ProcedureCall ("ivar_" ++ s) KIVar [p] () ()
          where
            p = Out (VarExpr v ()) ()
        destrs = map (ivarFun "destroy") iVars
