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



import Control.Monad.RWS

import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Array
import Feldspar.Core.Constructs.Literal

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation



instance ( Compile dom dom
         , Lambda TypeCtx :<: dom
         , Literal TypeCtx :<: dom
         , Variable TypeCtx :<: dom
         )
      => Compile Array dom
  where
    compileProgSym Parallel _ loc (len :* (lam :$ ixf) :* Nil)
        | Just (info, Lambda v) <- prjDecorCtx typeCtx lam
        = do
            let ta = argType $ infoType info
            let sa = defaultSize ta
            let ix@(Var _ name) = mkVar (compileTypeRep ta sa) v
            len' <- mkLength len
            (e, Bl ds body) <- confiscateBlock $ compileProg (loc :!: ix) ixf
            tellProg [initArray loc len']
            tellProg [For name len' 1 (Block ds body)]


    compileProgSym Sequential _ loc (len :* st :* (lam1 :$ (lam2 :$ step)) :* Nil)
        | Just (info1, Lambda v) <- prjDecorCtx typeCtx lam1
        , Just (info2, Lambda s) <- prjDecorCtx typeCtx lam2
        = do
            let t = argType $ infoType info1
            let sz = defaultSize t
            let ta' = argType $ infoType info2
            let sa' = defaultSize ta'
            let tr' = resType $ infoType info2
            let sr' = defaultSize tr'
            let ix@(Var _ name) = mkVar (compileTypeRep t sz) v
            let stv = mkVar (compileTypeRep ta' sa') s
            len' <- mkLength len
            tmp       <- freshVar "seq" tr' sr'
            initSt    <- compileExpr st
            (e, Bl ds (Seq body)) <- confiscateBlock $ compileProg tmp step
            tellProg [initArray loc len']
            tellProg [Block (ds ++ [ini stv initSt]) $
                      For name len' 1 $
                                    Seq (body ++
                                         [assignProg (loc :!: ix) (tmp :.: "member1")
                                         ,assignProg stv (tmp :.: "member2")
                                         ])]
      where def (Var ty str)   = Def  ty str
            ini (Var ty str) e = Init ty str e

    compileProgSym Append _ loc (a :* b :* Nil) = do
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

    compileProgSym SetIx info loc (arr :* i :* a :* Nil) = do
        compileProg loc arr
        i' <- compileExpr i
        compileProg (loc :!: i') a

    compileProgSym SetLength _ loc (len :* arr :* Nil) = do
        len' <- compileExpr len
        tellProg [setLength loc len']
        compileProg loc arr
      -- TODO Optimize by using copyProgLen (compare to 0.4)

    compileProgSym a info loc args = compileExprLoc a info loc args

    compileExprSym GetLength info (a :* Nil) = do
        aExpr <- compileExpr a
        return $ Fun (compileTypeRep (infoType info) (infoSize info)) "getLength" [aExpr]

    compileExprSym GetIx info (arr :* i :* Nil) = do
        a' <- compileExpr arr
        i' <- compileExpr i
        return $ a' :!: i'

    compileExprSym a info args = compileProgFresh a info args

