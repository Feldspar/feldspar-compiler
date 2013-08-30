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

module Feldspar.Compiler.Imperative.FromCore.MutableToPure where



import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Constructs.MutableArray
import Feldspar.Core.Constructs.MutableToPure

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.Representation (Expression(..))
import Feldspar.Compiler.Imperative.FromCore.Interpretation



instance ( Compile dom dom
         , Project (CLambda Type) dom
         , Project (Variable :|| Type) dom
         , Project MutableArray dom
         )
      => Compile MutableToPure dom
  where
    compileProgSym WithArray _ loc (marr :* (lam :$ body) :* Nil)
        | Just (C' (Variable _))        <- prjF marr
        , Just (SubConstr2 (Lambda v1)) <- prjLambda lam
        = do
            e <- compileExpr marr
            withAlias v1 e $ do
              b <- compileExpr body
              tellProg [copyProg loc [b]]

    compileProgSym WithArray _ loc (marr :* (lam :$ body) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
        = do
            let ta = argType $ infoType $ getInfo lam
            let sa = fst $ infoSize $ getInfo lam
            let var = mkVar (compileTypeRep ta sa) v
            declare var
            compileProg (Just var) marr
            e <- compileExpr body
            tellProg [copyProg loc [e]]

    compileProgSym RunMutableArray _ (Just loc) ((bnd :$ (na :$ l) :$ (lam :$ body)) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
        , Just NewArr_ <- prj na
        = do
            len <- compileExpr l
            tellProg [setLength loc len]
            withAlias v loc $ compileProg (Just loc) body

    compileProgSym RunMutableArray _ loc (marr :* Nil) = compileProg loc marr

{- NOTES:

The trick of doing a copy at the end, i.e. `tellProg [copyProg loc
e]`, when compiling WithArray is important. It allows us to safely
return the pure array that is passed in as input. This is nice because
it allows us to implement `freezeArray` in terms of `withArray`.
In most cases I expect `withArray` to return a scalar as its final
result and then the copyProg is harmless.
-}
