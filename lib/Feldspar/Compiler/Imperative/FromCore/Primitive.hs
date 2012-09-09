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

module Feldspar.Compiler.Imperative.FromCore.Primitive where



import Language.Syntactic
import Language.Syntactic.Interpretation.Semantics

import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Bits
import Feldspar.Core.Constructs.Complex
import Feldspar.Core.Constructs.Conversion
import Feldspar.Core.Constructs.Eq
import Feldspar.Core.Constructs.Floating
import Feldspar.Core.Constructs.Fractional
import Feldspar.Core.Constructs.Integral
import Feldspar.Core.Constructs.Logic
import Feldspar.Core.Constructs.Num
import Feldspar.Core.Constructs.Ord
import Feldspar.Core.Constructs.Trace

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation



-- | Converts symbols to primitive function calls
instance Compile dom dom => Compile Semantics dom
  where
    compileExprSym (Sem name _) info args = do
        argExprs <- sequence $ listArgs compileExpr args
        return $ Fun (compileTypeRep (infoType info) (infoSize info)) name argExprs

-- | Convenient implementation of 'compileExprSym' for primitive functions
compilePrim :: (Semantic expr, Compile dom dom)
    => expr a
    -> Info (DenResult a)
    -> Args (AST (Decor Info dom)) a
    -> CodeWriter Expr
compilePrim = compileExprSym . semantics

instance Compile dom dom => Compile BITS       dom where compileExprSym = compilePrim
instance Compile dom dom => Compile COMPLEX    dom where compileExprSym = compilePrim
instance Compile dom dom => Compile Conversion dom where compileExprSym = compilePrim
instance Compile dom dom => Compile EQ         dom where compileExprSym = compilePrim
instance Compile dom dom => Compile FLOATING   dom where compileExprSym = compilePrim
instance Compile dom dom => Compile FRACTIONAL dom where compileExprSym = compilePrim
instance Compile dom dom => Compile INTEGRAL   dom where compileExprSym = compilePrim
instance Compile dom dom => Compile Logic      dom where compileExprSym = compilePrim
instance Compile dom dom => Compile NUM        dom where compileExprSym = compilePrim
instance Compile dom dom => Compile ORD        dom where compileExprSym = compilePrim
instance Compile dom dom => Compile Trace      dom where compileExprSym = compilePrim

