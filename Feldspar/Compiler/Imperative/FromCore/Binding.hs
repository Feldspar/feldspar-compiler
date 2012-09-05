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

{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Compiler.Imperative.FromCore.Binding where

import Control.Monad.RWS

import Language.Syntactic

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding
import qualified Feldspar.Core.Constructs.Binding as Core

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation



instance Compile (Core.Variable TypeCtx) dom
  where
    compileExprSym (Core.Variable v) info Nil = do
        env <- ask
        case lookup v (alias env) of
          Nothing -> return $ Var (compileTypeRep (infoType info) (infoSize info)) ("v" ++ show v)
          Just e  -> return e

instance Compile (Lambda TypeCtx) dom
  where
    compileProgSym = error "Can only compile top-level Lambda"

instance (Compile dom dom, Lambda TypeCtx :<: dom) => Compile (Let TypeCtx TypeCtx) dom
  where
    compileProgSym Let _ loc (a :* (lam :$ body) :* Nil)
        | Just (info, Lambda v) <- prjDecorCtx typeCtx lam
        = do
            let ta = argType $ infoType info
            let sa = defaultSize ta
            let var = mkVar (compileTypeRep ta sa) v
            declare var
            compileProg var a
            compileProg loc body

    compileExprSym Let _ (a :* (lam :$ body) :* Nil)
        | Just (info, Lambda v) <- prjDecorCtx typeCtx lam
        = do
            let ta = argType $ infoType info
            let sa = defaultSize ta
            let var = mkVar (compileTypeRep ta sa) v
            declare var
            compileProg var a
            compileExpr body

