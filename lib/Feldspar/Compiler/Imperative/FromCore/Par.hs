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

module Feldspar.Compiler.Imperative.FromCore.Par where

import Language.Syntactic
import Language.Syntactic.Constructs.Monad
import Language.Syntactic.Constructs.Binding hiding (Variable)
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding hiding (Variable)
import Feldspar.Core.Constructs.Par

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.Representation (Block(..), Program(..),
                                                    Entity(..), typeof)
import Feldspar.Compiler.Imperative.FromCore.Interpretation
import qualified Feldspar.Compiler.Imperative.Representation as AIR

import Data.Map (assocs)

instance ( Compile dom dom
         , Project (CLambda Type) dom
         , Project ParFeature dom
         )
      => Compile (MONAD Par) dom
  where
    compileProgSym Bind _ loc (ma :* (lam :$ body) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
        , Just ParNew                  <- prj ma
        = do
            let info = getInfo ma
            let var = mkVar (compileTypeRep (infoType info) (infoSize info)) v
            declare var
            tellProg [iVarInit var]
            compileProg loc body

    compileProgSym Bind _ loc (ma :* (lam :$ body) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
        = do
            e <- compileExpr ma
            withAlias v e $ compileProg loc body

    compileProgSym Then _ loc (ma :* mb :* Nil) = do
        compileExpr ma
        compileProg loc mb

    compileProgSym Return info loc (a :* Nil)
        | ParType UnitType <- infoType info = return ()
        | otherwise                         = compileProg loc a

    compileProgSym When _ loc (c :* action :* Nil) = do
        c' <- compileExpr c
        (_, b) <- confiscateBlock $ compileProg loc action
        tellProg [Branch c' b (toBlock Empty)]

instance ( Compile dom dom
         , Project (Variable :|| Type) dom
         )
      => Compile ParFeature dom
  where
    compileExprSym = compileProgFresh

    compileProgSym ParRun _ loc (p :* Nil) = compileProg loc p

    compileProgSym ParGet _ loc (r :* Nil) = do
        iv <- compileExpr r
        tellProg [iVarGet loc iv]

    compileProgSym ParPut _ _ (r :* a :* Nil) = do
            iv  <- compileExpr r
            val <- compileExpr a
            i   <- freshId
            let var = varToExpr $ mkNamedVar "msg" (typeof val) i
            declare var
            assign var val
            tellProg [iVarPut iv var]

    compileProgSym ParFork info loc (p :* Nil) = do
        let args = [mkVariable (compileTypeRep t (defaultSize t)) v
                   | (v,SomeType t) <- assocs $ infoVars info
                   ]
        -- Task core:
        (_, b) <- confiscateBlock $ compileProg loc p
        funId  <- freshId
        let coreName = "task_core" ++ show funId
        tellDef [ProcDef coreName args [] b]
        -- Task:
        let taskName = "task" ++ show funId
        let runTask = run coreName args
        parId <- freshId
        tellDef [ProcDef taskName [] [mkNamedVar "params" AIR.VoidType parId] $ toBlock runTask]
        -- Spawn:
        tellProg [spawn taskName args]

    compileProgSym ParYield _ _ Nil = return ()

    compileProgSym ParNew _ _ Nil = return ()


