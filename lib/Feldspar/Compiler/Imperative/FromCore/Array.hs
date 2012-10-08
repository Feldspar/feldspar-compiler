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

module Feldspar.Compiler.Imperative.FromCore.Array where



import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types as Core
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Array
import Feldspar.Core.Constructs.Literal

import Feldspar.Compiler.Imperative.Frontend hiding (Type)
import Feldspar.Compiler.Imperative.FromCore.Interpretation



instance ( Compile dom dom
         , Project (SubConstr2 (->) Lambda Type Top) dom
         , Project (Literal  :|| Type) dom
         , Project (Variable :|| Type) dom
         )
      => Compile (Array :|| Type) dom
  where
    compileProgSym (C' Parallel) _ loc (len :* (lam :$ ixf) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjP (P::P (SubConstr2 (->) Lambda Type Top)) lam  -- TODO Use helper function
        = do
            let ta = argType $ infoType $ getInfo lam
            let sa = defaultSize ta
            let ix@(Var _ name) = mkVar (compileTypeRep ta sa) v
            len' <- mkLength len
            (_, Bl ds body) <- confiscateBlock $ compileProg (loc :!: ix) ixf
            tellProg [initArray loc len']
            tellProg [For name len' 1 (Block ds body)]


    compileProgSym (C' Sequential) _ loc (len :* st :* (lam1 :$ (lam2 :$ step)) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjP (P::P (SubConstr2 (->) Lambda Type Top)) lam1
        , Just (SubConstr2 (Lambda s)) <- prjP (P::P (SubConstr2 (->) Lambda Type Top)) lam2
        = do
            let t = argType $ infoType $ getInfo lam1
            let sz = defaultSize t
            let ta' = argType $ infoType $ getInfo lam2
            let sa' = defaultSize ta'
            let tr' = resType $ infoType $ getInfo lam2
            let sr' = defaultSize tr'
            let ix@(Var _ name) = mkVar (compileTypeRep t sz) v
            let stv = mkVar (compileTypeRep ta' sa') s
            len' <- mkLength len
            tmp       <- freshVar "seq" tr' sr'
            initSt    <- compileExpr st
            (_, Bl ds (Seq body)) <- confiscateBlock $ compileProg tmp step
            tellProg [initArray loc len']
            tellProg [Block (ds ++ [toIni stv initSt]) $
                      For name len' 1 $
                                    Seq (body ++
                                         [assignProg (loc :!: ix) (tmp :.: "member1")
                                         ,assignProg stv (tmp :.: "member2")
                                         ])]
      where toIni (Var ty str) = Init ty str

    compileProgSym (C' Append) _ loc (a :* b :* Nil) = do
        a' <- compileExpr a
        b' <- compileExpr b
        let aLen = arrayLength a'
        let bLen = arrayLength b'
        tellProg [initArray loc $ Binop U32 "+" [aLen, bLen]]
        tellProg [copyProg loc a']
        tellProg [copyProgPos loc aLen b']
        -- TODO: Optimize by writing to directly to 'loc' instead of 'a'' and 'b''!
        --       But take care of array initialization:
        --       compiling 'a' and 'b' might do initialization itself...

    compileProgSym (C' SetIx) _ loc (arr :* i :* a :* Nil) = do
        compileProg loc arr
        i' <- compileExpr i
        compileProg (loc :!: i') a

    compileProgSym (C' SetLength) _ loc (len :* arr :* Nil) = do
        len' <- compileExpr len
        tellProg [setLength loc len']
        compileProg loc arr
      -- TODO Optimize by using copyProgLen (compare to 0.4)

    compileProgSym a info loc args = compileExprLoc a info loc args

    compileExprSym (C' GetLength) info (a :* Nil) = do
        aExpr <- compileExpr a
        return $ Fun (compileTypeRep (infoType info) (infoSize info)) "getLength" [aExpr]

    compileExprSym (C' GetIx) _ (arr :* i :* Nil) = do
        a' <- compileExpr arr
        i' <- compileExpr i
        return $ a' :!: i'

    compileExprSym a info args = compileProgFresh a info args

