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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore.Future where

import Language.Syntactic

import Feldspar.Core.Types (Type)
import Feldspar.Core.Constructs.Future

import Feldspar.Compiler.Imperative.Frontend hiding (Type)
import qualified Feldspar.Compiler.Imperative.Frontend as Front
import Feldspar.Compiler.Imperative.FromCore.Interpretation
import Feldspar.Compiler.Imperative.Plugin.CollectFreeVars
import Feldspar.Transformation (transform, Result(..))

import Data.Map (elems)

instance Compile dom dom => Compile (FUTURE :|| Type) dom
  where
    compileExprSym = compileProgFresh

    compileProgSym (C' MkFuture) _ loc (p :* Nil) = do
        -- Task core:
        (_, Bl ds t)  <- confiscateBlock $ do
            p' <- compileExprVar p
            tellProg [iVarPut loc p']
        let b = Block ds t
        let vs = elems $ up $ transform Collect () () $ Front.fromInterface b
        funId  <- freshId
        let coreName = "task_core" ++ show funId
        tellDef [ProcDf coreName vs [] b]
        -- Task:
        let taskName = "task" ++ show funId
        let runTask = run coreName vs
        tellDef [ProcDf taskName [] [Front.Variable Void "params"] runTask]
        -- Spawn:
        tellProg [iVarInit loc]
        tellProg [spawn taskName vs]

    compileProgSym (C' Await) _ loc (a :* Nil) = do
        fut <- compileExprVar a -- compileExpr a
        tellProg [iVarGet loc fut]

