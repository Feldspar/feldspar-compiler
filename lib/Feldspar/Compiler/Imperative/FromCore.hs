{-# LANGUAGE ConstraintKinds #-}

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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Feldspar.Compiler.Imperative.FromCore where


import Data.List (nub)

import Control.Monad.RWS

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Frontend

import Feldspar.Compiler.Imperative.Representation as Rep (Variable(..))
import Feldspar.Compiler.Imperative.Representation (Expression(..), Program(..), Block(..), Module(..), Entity(..), VariableRole(..))
import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation
import Feldspar.Compiler.Imperative.FromCore.Array ()
import Feldspar.Compiler.Imperative.FromCore.Binding ()
import Feldspar.Compiler.Imperative.FromCore.Condition ()
import Feldspar.Compiler.Imperative.FromCore.ConditionM ()
import Feldspar.Compiler.Imperative.FromCore.Error ()
import Feldspar.Compiler.Imperative.FromCore.FFI ()
import Feldspar.Compiler.Imperative.FromCore.Future ()
import Feldspar.Compiler.Imperative.FromCore.Literal ()
import Feldspar.Compiler.Imperative.FromCore.Loop ()
import Feldspar.Compiler.Imperative.FromCore.Mutable ()
import Feldspar.Compiler.Imperative.FromCore.MutableToPure ()
import Feldspar.Compiler.Imperative.FromCore.NoInline ()
import Feldspar.Compiler.Imperative.FromCore.Par ()
import Feldspar.Compiler.Imperative.FromCore.Primitive ()
import Feldspar.Compiler.Imperative.FromCore.Save ()
import Feldspar.Compiler.Imperative.FromCore.SizeProp ()
import Feldspar.Compiler.Imperative.FromCore.SourceInfo ()
import Feldspar.Compiler.Imperative.FromCore.Tuple ()

import Feldspar.Compiler.Backend.C.Options (Options(..))

instance Compile FeldDom FeldDom
  where
    compileProgSym (C' a) = compileProgSym a
    compileExprSym (C' a) = compileExprSym a

instance Compile Empty dom
  where
    compileProgSym _ = error "Can't compile Empty"
    compileExprSym _ = error "Can't compile Empty"

compileProgTop :: (Compile dom dom, Project (CLambda Type) dom) =>
    Options -> String -> [((VarId,Expression ()),Rep.Variable ())] -> ASTF (Decor Info dom) a -> Module ()
compileProgTop opt funname args (lam :$ body)
    | Just (SubConstr2 (Lambda v)) <- prjLambda lam
    = let ta  = argType $ infoType $ getInfo lam
          sa  = fst $ infoSize $ getInfo lam
          typ = compileTypeRep ta sa
          arg = if isComposite typ
                  then ((v, mkRef typ v), mkPointer  typ v)
                  else ((v, mkVar typ v), mkVariable typ v)
       in compileProgTop opt funname (arg:args) body
compileProgTop opt funname args a = Module defs
  where
    ins      = map snd $ reverse args
    info     = getInfo a
    outType  = compileTypeRep (infoType info) (infoSize info)
    outParam = Rep.Variable Ptr outType "out"
    outLoc   = varToExpr outParam
    results  = snd $ evalRWS (compileProg outLoc a) (initReader opt){alias=map fst args} initState
    decls    = decl results
    post     = epilogue results
    Block ds p = block results
    defs     = (nub $ def results) ++ [ProcDef funname ins [outParam] (Block (ds ++ decls) (Sequence (p:post)))]

fromCore :: SyntacticFeld a => Options -> String -> a -> Module ()
fromCore opt funname
    = compileProgTop opt funname []
    . reifyFeld defaultFeldOpts N32

-- | Get the generated core for a program.
getCore' :: SyntacticFeld a => Options -> a -> Module ()
getCore' opts prog = compileProgTop opts "test" [] (reifyFeld defaultFeldOpts N32 prog)
