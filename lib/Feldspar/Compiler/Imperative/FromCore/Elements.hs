{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore.Elements where

import Data.Typeable

import Language.Syntactic
import Language.Syntactic.Constructs.Binding hiding (Variable)
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding hiding (Variable)
import Feldspar.Core.Constructs.Elements
import Feldspar.Core.Constructs.Literal

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.Representation (Block(..), Program(..),
                                                    Expression(..))
import Feldspar.Compiler.Imperative.FromCore.Interpretation

instance ( Compile dom dom
         , Render dom
         , Project (CLambda Type) dom
         , Project (Variable :|| Type) dom
         , Project (Literal :|| Type) dom
         , Project Let dom
         , Project (ElementsFeat :|| Type) dom
         )
      => Compile (ElementsFeat :|| Type) dom
  where
    compileExprSym a info args = compileProgFresh a info args

    compileProgSym (C' EMaterialize) _ loc (len :* arr :* Nil) = do
      let sz  = infoSize $ getInfo len
      len' <- mkLength len (infoType $ getInfo len) sz
      tellProg [initArray loc len']
      compileProg loc arr

    compileProgSym (C' EWrite) _ (Just loc) (ix :* e :* Nil) = do
      dst <- compileExpr ix
      compileProg (Just $ ArrayElem loc dst) e

    compileProgSym (C' EPar) _ loc (p1 :* p2 :* Nil) = do
      (_, Block ds1 b1) <- confiscateBlock $ compileProg loc p1
      (_, Block ds2 b2) <- confiscateBlock $ compileProg loc p2
      tellProg [toProg $ Block (ds1 ++ ds2) (Sequence [b1,b2])]

    compileProgSym (C' EparFor) info loc (len :* (lam :$ ixf) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
        = do
            let ta = argType $ infoType $ getInfo lam
            let sa = fst $ infoSize $ getInfo lam
            let ix = mkVar (compileTypeRep ta sa) v
            len' <- mkLength len (infoType $ getInfo len) sa
            (_, ixf') <- confiscateBlock $ compileProg loc ixf
            tellProg [for True (lName ix) len' (litI32 1) ixf']

    compileProgSym (C' ESkip) _ _ _ = tellProg [Comment True "Skip"]

    compileProgSym a info loc args = compileExprLoc a info loc args
