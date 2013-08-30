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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore where


import Data.List (nub)
import Data.Typeable

import Control.Monad.RWS

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Literal
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Frontend

import Feldspar.Range (upperBound)

import qualified Feldspar.Compiler.Imperative.Representation as Rep (Variable(..), Type(..))
import Feldspar.Compiler.Imperative.Representation (ActualParameter(..), Expression(..), Program(..), Block(..), Module(..), Entity(..), Declaration(..))
import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation
import Feldspar.Compiler.Imperative.FromCore.Array ()
import Feldspar.Compiler.Imperative.FromCore.Binding (compileBind)
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

compileProgTop :: ( Compile dom dom
                  , Project (CLambda Type) dom
                  , Project Let dom
                  , Project (Literal :|| Type) dom
                  , ConstrainedBy dom Typeable
                  ) =>
    Options -> String -> [(VarId, ASTB (Decor Info dom) Type)] ->
    ASTF (Decor Info dom) a -> CodeWriter (Rep.Variable ())
compileProgTop opt funname bs (lam :$ body)
    | Just (SubConstr2 (Lambda v)) <- prjLambda lam
    = do
         let ta  = argType $ infoType $ getInfo lam
             sa  = fst $ infoSize $ getInfo lam
             typ = compileTypeRep ta sa
             arg | Rep.StructType{} <- typ = mkPointer typ v
                 | otherwise               = mkVariable typ v
         tell $ mempty {args=[arg]}
         withAlias v (varToExpr arg) $
           compileProgTop opt funname bs body
compileProgTop opt funname bs (lt :$ e :$ (lam :$ body))
  | Just (SubConstr2 (Lambda v)) <- prjLambda lam
  , Just Let <- prj lt
  , Just (C' Literal{}) <- prjF e -- Input on form let x = n in e
  , [ProcedureCall "copy" [ValueParameter (VarExpr vr), ValueParameter (ConstExpr c)]] <- bd
  , freshName Prelude.== vName vr -- Ensure that compiled result is on form x = n
  = do tellDef [ValueDef var c]
       withAlias v (varToExpr var) $
         compileProgTop opt funname bs body
  where
    info     = getInfo e
    outType  = case compileTypeRep (infoType info) (infoSize info) of
                 Rep.ArrayType rs t -> Rep.NativeArray (Just $ upperBound rs) t
                 t -> t
    var@(Rep.Variable _ freshName) = case prjLambda lam of
               Just (SubConstr2 (Lambda v)) -> mkVariable outType v
    bd = sequenceProgs $ blockBody $ block $ snd $
          evalRWS (compileProg (Just $ varToExpr var) e) (initReader opt) initState
compileProgTop opt funname bs e@(lt :$ _ :$ _)
  | Just Let <- prj lt
  , (bs', body) <- collectLetBinders e
  = compileProgTop opt funname (reverse bs' ++ bs) body
compileProgTop opt funname bs a = do
    let
        info       = getInfo a
        outType    = Rep.Pointer $ compileTypeRep (infoType info) (infoSize info)
        outParam   = Rep.Variable outType "out"
        outLoc     = varToExpr outParam
    mapM compileBind (reverse bs)
    compileProg (Just outLoc) a
    return outParam

fromCore :: SyntacticFeld a => Options -> String -> a -> Module ()
fromCore opt funname prog = Module defs
  where
    (outParam,results) = evalRWS (compileProgTop opt funname [] ast) (initReader opt) initState
    ast        = reifyFeld (frontendOpts opt) N32 prog
    decls      = decl results
    ins        = args results
    post       = epilogue results
    Block ds p = block results
    paramTypes = getTypes opt $ Declaration outParam Nothing:map (\v -> Declaration v Nothing) ins
    defs       =  nub (def results ++ paramTypes)
               ++ [ProcDef funname ins [outParam] (Block (ds ++ decls) (Sequence (p:post)))]

-- | Get the generated core for a program.
getCore' :: SyntacticFeld a => Options -> a -> Module ()
getCore' opts = fromCore opts "test"
