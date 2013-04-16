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
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore.NoInline where

import Data.Map (assocs)
import Data.List (partition)
import Language.Syntactic

import Feldspar.Core.Types (Type,defaultSize)
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.NoInline
import Feldspar.Compiler.Imperative.Representation (Variable(..),
                                                    Program(..),
                                                    ActualParameter(..),
                                                    Entity(..), typeof)
import Feldspar.Compiler.Imperative.FromCore.Interpretation
import Feldspar.Compiler.Imperative.Frontend

instance Compile dom dom => Compile (NoInline :|| Type) dom
  where
    compileExprSym = compileProgFresh

    compileProgSym (C' NoInline) info loc (p :* Nil) = do
        let args = [mkVariable (compileTypeRep t (defaultSize t)) v
                   | (v,SomeType t) <- assocs $ infoVars info
                   ]
        (_, b)  <- confiscateBlock $ compileProg loc p
        let isInParam v = vName v /= lName loc
        let (ins,outs) = partition isInParam args
        funId  <- freshId
        let funname = "noinline" ++ show funId
        tellDef [ProcDef funname ins outs b]
        let ins' = map (\v -> ValueParameter $ varToExpr $ Variable (typeof v) (vName v)) ins
        tellProg [call funname $ ins' ++ [ValueParameter loc]]

