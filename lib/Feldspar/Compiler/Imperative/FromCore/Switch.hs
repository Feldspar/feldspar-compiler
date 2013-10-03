{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore.Switch where

import Language.Syntactic

import Feldspar.Core.Types (Type)
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Array
import Feldspar.Core.Constructs.Switch
import Feldspar.Core.Constructs.Tuple

import Feldspar.Compiler.Imperative.Frontend
import qualified Feldspar.Compiler.Imperative.Representation as R
import Feldspar.Compiler.Imperative.FromCore.Interpretation

instance ( Compile dom dom
         , Project (Array :|| Type) dom
         , Project (Tuple :|| Type) dom
         )
      => Compile (Switch :|| Type) dom
  where
    compileProgSym (C' Switch) _ loc (s :* def :* cs :* Nil) = do
        scrutinee   <- compileExpr s
        (ds,defaultProg) <- confiscateBlock $ compileProg loc def
        cases <- compileCases loc cs
        let alts = (R.PatDefault, defaultProg) : cases
        tellProg [R.Switch{..}]

compileCases :: ( Compile dom dom
                , Project (Array :|| Type) dom
                , Project (Tuple :|| Type) dom
                )
             => Location -> AST (Decor Info dom) a -> CodeWriter [(R.Pattern (), R.Block ())]
compileCases loc (append :$ c :$ cs) = do x  <- compileCase loc c
                                          xs <- compileCases loc cs
                                          return (x:xs)
compileCases _   _                   = return []

compileCase loc (arr :$ _ :$ (lam :$ (tup :$ a :$ b)))
    | Just (C' Parallel) <- prjF arr
    , Just (C' Tup2)     <- prjF tup
    = do
         e      <- compileExpr a
         (_,p) <- confiscateBlock $ compileProg loc b
         return (R.Pat (R.BoolConst False),p)
