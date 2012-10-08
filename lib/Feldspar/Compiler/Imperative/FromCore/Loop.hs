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

module Feldspar.Compiler.Imperative.FromCore.Loop where


import Prelude hiding (init)

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Loop hiding (For, While)
import Feldspar.Core.Constructs.Literal
import qualified Feldspar.Core.Constructs.Loop as Core

import Feldspar.Compiler.Imperative.Frontend hiding (Type)
import Feldspar.Compiler.Imperative.FromCore.Interpretation

instance ( Compile dom dom
         , Project (SubConstr2 (->) Lambda Type Top) dom
         , Project (Literal  :|| Type) dom
         , Project (Variable :|| Type) dom
         )
      => Compile (Loop :|| Type) dom
  where
    compileProgSym (C' ForLoop) _ loc (len :* init :* (lam1 :$ (lam2 :$ ixf)) :* Nil)
        | Just (SubConstr2 (Lambda ix)) <- prjP (P::P (SubConstr2 (->) Lambda Type Top)) lam1
        , Just (SubConstr2 (Lambda st)) <- prjP (P::P (SubConstr2 (->) Lambda Type Top)) lam2
        = do
            let info1 = getInfo lam1
                info2 = getInfo lam2
            let (Var _ name) = mkVar (compileTypeRep (infoType info1) (infoSize info1)) ix
            let stvar        = mkVar (compileTypeRep (infoType info2) (infoSize info2)) st
            len' <- mkLength len
            compileProg loc init
            (_, Bl ds body) <- withAlias st loc $ confiscateBlock $ compileProg stvar ixf >> assign loc stvar
            declare stvar
            tellProg [For name len' 1 (Block ds body)]

    compileProgSym (C' WhileLoop) _ loc (init :* (lam1 :$ cond) :* (lam2 :$ body) :* Nil)
        | Just (SubConstr2 (Lambda cv)) <- prjP (P::P (SubConstr2 (->) Lambda Type Top)) lam1
        , Just (SubConstr2 (Lambda cb)) <- prjP (P::P (SubConstr2 (->) Lambda Type Top)) lam2
        = do
            let info2 = getInfo lam2
            let stvar = mkVar (compileTypeRep (infoType info2) (infoSize info2)) cb
            compileProg loc init
            cond' <- withAlias cv loc $ compileExpr cond
            (_, Bl ds body') <- withAlias cb loc $ confiscateBlock $ compileProg stvar body >> assign loc stvar
            declare stvar
            tellProg [While Skip cond' (Block ds body')]

instance ( Compile dom dom
         , Project (SubConstr2 (->) Lambda Type Top) dom
         , Project (Literal  :|| Type) dom
         , Project (Variable :|| Type) dom
         )
      => Compile (LoopM Mut) dom
  where
    compileProgSym Core.For _ loc (len :* (lam :$ ixf) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjP (P::P (SubConstr2 (->) Lambda Type Top)) lam
        = do
            let ta = argType $ infoType $ getInfo lam
            let sa = defaultSize ta
            let (Var _ name) = mkVar (compileTypeRep ta sa) v
            len' <- mkLength len
            (_, Bl _ body) <- confiscateBlock $ compileProg loc ixf
            tellProg [For name len' 1 body]

-- TODO Missing While
    compileProgSym Core.While _ loc (cond :* step :* Nil)
        = do
            cond'     <- compileExpr cond
            (_, Bl _ step') <- confiscateBlock $ compileProg loc step
            tellProg [While Skip cond' step']

