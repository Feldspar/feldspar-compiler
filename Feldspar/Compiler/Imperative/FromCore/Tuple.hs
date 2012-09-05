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

module Feldspar.Compiler.Imperative.FromCore.Tuple where



import Language.Syntactic

import Feldspar.Core.Types
import Feldspar.Core.Constructs.Tuple

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation



instance Compile dom dom => Compile (Tuple TypeCtx) dom
  where
    compileProgSym Tup2 _ loc (m1 :* m2 :* Nil) = do
        compileExpr m1 >>= assign (loc :.: "member1")
        compileExpr m2 >>= assign (loc :.: "member2")
    compileProgSym Tup3 _ loc (m1 :* m2 :* m3 :* Nil) = do
        compileExpr m1 >>= assign (loc :.: "member1")
        compileExpr m2 >>= assign (loc :.: "member2")
        compileExpr m3 >>= assign (loc :.: "member3")
    compileProgSym Tup4 _ loc (m1 :* m2 :* m3 :* m4 :* Nil) = do
        compileExpr m1 >>= assign (loc :.: "member1")
        compileExpr m2 >>= assign (loc :.: "member2")
        compileExpr m3 >>= assign (loc :.: "member3")
        compileExpr m4 >>= assign (loc :.: "member4")
    compileProgSym Tup5 _ loc (m1 :* m2 :* m3 :* m4 :* m5 :* Nil) = do
        compileExpr m1 >>= assign (loc :.: "member1")
        compileExpr m2 >>= assign (loc :.: "member2")
        compileExpr m3 >>= assign (loc :.: "member3")
        compileExpr m4 >>= assign (loc :.: "member4")
        compileExpr m5 >>= assign (loc :.: "member5")
    compileProgSym Tup6 _ loc (m1 :* m2 :* m3 :* m4 :* m5 :* m6 :* Nil) = do
        compileExpr m1 >>= assign (loc :.: "member1")
        compileExpr m2 >>= assign (loc :.: "member2")
        compileExpr m3 >>= assign (loc :.: "member3")
        compileExpr m4 >>= assign (loc :.: "member4")
        compileExpr m5 >>= assign (loc :.: "member5")
        compileExpr m6 >>= assign (loc :.: "member6")
    compileProgSym Tup7 _ loc (m1 :* m2 :* m3 :* m4 :* m5 :* m6 :* m7 :* Nil) = do
        compileExpr m1 >>= assign (loc :.: "member1")
        compileExpr m2 >>= assign (loc :.: "member2")
        compileExpr m3 >>= assign (loc :.: "member3")
        compileExpr m4 >>= assign (loc :.: "member4")
        compileExpr m5 >>= assign (loc :.: "member5")
        compileExpr m6 >>= assign (loc :.: "member6")
        compileExpr m7 >>= assign (loc :.: "member7")

instance Compile dom dom => Compile (Select TypeCtx) dom
  where
    compileExprSym Sel1 _ (tup :* Nil) = do
        tupExpr <- compileExpr tup
        return $ tupExpr :.: "member1"
    compileExprSym Sel2 _ (tup :* Nil) = do
        tupExpr <- compileExpr tup
        return $ tupExpr :.: "member2"
    compileExprSym Sel3 _ (tup :* Nil) = do
        tupExpr <- compileExpr tup
        return $ tupExpr :.: "member3"
    compileExprSym Sel4 _ (tup :* Nil) = do
        tupExpr <- compileExpr tup
        return $ tupExpr :.: "member4"
    compileExprSym Sel5 _ (tup :* Nil) = do
        tupExpr <- compileExpr tup
        return $ tupExpr :.: "member5"
    compileExprSym Sel6 _ (tup :* Nil) = do
        tupExpr <- compileExpr tup
        return $ tupExpr :.: "member6"
    compileExprSym Sel7 _ (tup :* Nil) = do
        tupExpr <- compileExpr tup
        return $ tupExpr :.: "member7"

