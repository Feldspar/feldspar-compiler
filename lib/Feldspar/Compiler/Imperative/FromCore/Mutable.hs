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

module Feldspar.Compiler.Imperative.FromCore.Mutable where


import Control.Applicative

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Constructs.Mutable
import Feldspar.Core.Constructs.MutableArray
import Feldspar.Core.Constructs.MutableReference

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation
import qualified Feldspar.Compiler.Imperative.Representation as Rep (Type(..),
                                                                     Size(..),
                                                                     Signedness(..))
import Feldspar.Compiler.Imperative.Representation (Expression(..),
                                                    Program(..))

instance ( Compile dom dom
         , Project (CLambda Type) dom
         )
      => Compile (MONAD Mut) dom
  where
    compileProgSym Bind _ loc (ma :* (lam :$ body) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
        = do
            let info = getInfo ma
                var  = mkVar (compileTypeRep (infoType info) (infoSize info)) v
            declare var
            compileProg (Just var) ma
            compileProg loc body

    compileProgSym Then _ loc (ma :* mb :* Nil) = do
        compileProg Nothing ma
        compileProg loc mb

    compileProgSym Return info loc (a :* Nil)
        | MutType UnitType <- infoType info = return ()
        | otherwise                         = compileProg loc a

    compileProgSym When _ loc (c :* action :* Nil) = do
        mkBranch loc c action Nothing

instance (Compile dom dom, Project (CLambda Type) dom) => Compile Mutable dom
  where
    compileProgSym Run _ loc (ma :* Nil) = compileProg loc ma

    compileExprSym Run _ (ma :* Nil) = compileExpr ma

instance (Compile dom dom, Project (CLambda Type) dom) => Compile MutableReference dom
  where
    compileProgSym NewRef _ loc (a :* Nil) = compileProg loc a
    compileProgSym GetRef _ loc (r :* Nil) = compileProg loc r
    compileProgSym SetRef _ _   (r :* a :* Nil) = do
        var  <- compileExpr r
        compileProg (Just var) a
    compileProgSym ModRef _ _ (r :* (lam :$ body) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
        = do
            var <- compileExpr r
            withAlias v var $ compileProg (Just var) body
               -- Since the modifier function is pure it is safe to alias
               -- v with var here

    compileExprSym GetRef _ (r :* Nil) = compileExpr r
    compileExprSym feat info args      = compileProgFresh feat info args

instance (Compile dom dom, Project (CLambda Type) dom) => Compile MutableArray dom
  where
    compileProgSym NewArr_ _ loc (len :* Nil) = do
      l <- compileExpr len
      tellProg [initArray loc l]

    compileProgSym NewArr _ loc (len :* a :* Nil) = do
        nId <- freshId
        let ix = varToExpr $ mkNamedVar "i" (Rep.NumType Rep.Unsigned Rep.S32) nId
        a' <- compileExpr a
        l  <- compileExpr len
        tellProg [initArray loc l]
        tellProg [for False "i" l (litI32 1) $ toBlock (Sequence [copyProg (ArrayElem <$> loc <*> pure ix) [a']])]

    compileProgSym GetArr _ loc (arr :* i :* Nil) = do
        arr' <- compileExpr arr
        i'   <- compileExpr i
        assign loc (ArrayElem arr' i')

    compileProgSym SetArr _ _ (arr :* i :* a :* Nil) = do
        arr' <- compileExpr arr
        i'   <- compileExpr i
        a'   <- compileExpr a
        assign (Just $ ArrayElem arr' i') a'

    compileProgSym a info loc args = compileExprLoc a info loc args

    compileExprSym ArrLength _ (arr :* Nil) = do
        a' <- compileExpr arr
        return $ arrayLength a'

    compileExprSym a info args = compileProgFresh a info args

