{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore.Switch where

import Control.Monad (forM)

import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Core.Types (Type,TypeRep(..))
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Eq
import Feldspar.Core.Constructs.Condition
import Feldspar.Core.Constructs.Switch

import Feldspar.Compiler.Imperative.Frontend
import qualified Feldspar.Compiler.Imperative.Representation as R
import Feldspar.Compiler.Imperative.FromCore.Interpretation

import Debug.Trace

instance ( Compile dom dom
         , Project (EQ :|| Type) dom
         , Project (Condition :|| Type) dom
         )
      => Compile (Switch :|| Type) dom
  where
    compileProgSym (C' Switch) _ loc (tree@(cond :$ (op :$ _ :$ s) :$ _ :$ _) :* Nil)
        | Just (C' Condition) <- prjF cond
        , Just (C' Equal)     <- prjF op
        = do
             scrutinee <- compileExpr s
             alts      <- chaseTree loc s tree
             tellProg [R.Switch{..}]

chaseTree :: ( Compile dom dom
             , Project (Condition :|| Type) dom
             , Project (EQ        :|| Type) dom
             )
          => Location -> ASTF (Decor Info dom) a -> ASTF (Decor Info dom) b -> CodeWriter [(R.Pattern (), R.Block ())]
chaseTree loc s (cond :$ (op :$ c :$ a) :$ t :$ f)
    | Just (C' Condition) <- prjF cond
    , Just (C' Equal)     <- prjF op
    -- , alphaEq s a -- TODO check that the scrutinees are equal
    = do
         e <- compileExpr c
         (_,body) <- confiscateBlock $ compileProg loc t
         cases <- chaseTree loc s f
         return $ (R.Pat e, body) : cases

chaseTree loc s a = do
    (_,body) <- confiscateBlock $ compileProg loc a
    return [(R.PatDefault, body)]

