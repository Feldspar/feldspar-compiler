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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore.Array where


import Data.List (init)
import Data.Typeable

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types as Core
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Array
import Feldspar.Core.Constructs.Tuple
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Constructs.Literal

import Feldspar.Compiler.Imperative.Frontend
import qualified Feldspar.Compiler.Imperative.Representation as Rep (Type(..))
import Feldspar.Compiler.Imperative.Representation (Expression(..), Program(..),
                                                    Block(..), Size(..),
                                                    Signedness(..))
import Feldspar.Compiler.Imperative.FromCore.Interpretation
import Feldspar.Compiler.Imperative.FromCore.Binding (compileBind)



instance ( Compile dom dom
         , Project (CLambda Type) dom
         , Project (Literal  :|| Type) dom
         , Project (Variable :|| Type) dom
         , Project Let dom
         , Project (Array :|| Type) dom
         , Project (Tuple :|| Type) dom
         , ConstrainedBy dom Typeable
         , AlphaEq dom dom (Decor Info dom) [(VarId, VarId)]
         )
      => Compile (Array :|| Type) dom
  where
    compileProgSym (C' Parallel) _ loc (len :* (lam :$ ixf) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
        = do
            let ta = argType $ infoType $ getInfo lam
            let sa = fst $ infoSize $ getInfo lam
            let ix = mkVar (compileTypeRep ta sa) v
            len' <- mkLength len (infoType $ getInfo len) sa
            (_, b) <- confiscateBlock $ compileProg (ArrayElem (AddrOf loc) ix) ixf
            tellProg [initArray (AddrOf loc) len']
            tellProg [for (lName ix) len' 1 b]


    compileProgSym (C' Sequential) _ loc (len :* init' :* (lam1 :$ (lam2 :$ l@(lt :$ _ :$ _))) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam1
        , Just (SubConstr2 (Lambda s)) <- prjLambda lam2
        , Just Let                     <- prj lt
        , (bs, (tup :$ a :$ b))        <- collectLetBinders l
        , Just (C' Tup2)               <- prjF tup
        , (e, ASTB step)               <- last bs
        , Just (C' (Variable t1))      <- prjF a
        , Just (C' (Variable t2))      <- prjF b
        , t1 == e
        , t2 == e
        = do
            mapM_ compileBind (init bs)
            let tix = argType $ infoType $ getInfo lam1
                six = fst $ infoSize $ getInfo lam1
                tst = infoType $ getInfo step
                sst = infoSize $ getInfo step
            let ix = mkVar (compileTypeRep tix six) v
            len' <- mkLength len (infoType $ getInfo len) six
            let st = mkVar (compileTypeRep tst sst) s
            declare st
            (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s st $ compileProg (ArrayElem (AddrOf loc) ix) step
            tellProg [initArray (AddrOf loc) len']
            withAlias s st $ compileProg st init'
            tellProg [toProg $ Block ds $
                      for (lName ix) len' 1 $
                                    toBlock $ Sequence (body ++
                                         [copyProg (AddrOf st) [AddrOf $ ArrayElem (AddrOf loc) ix]
                                         ])]

    compileProgSym (C' Sequential) _ loc (len :* st :* (lam1 :$ (lam2 :$ step)) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam1
        , Just (SubConstr2 (Lambda s)) <- prjLambda lam2
        = do
            let t = argType $ infoType $ getInfo lam1
            let sz = fst $ infoSize $ getInfo lam1
            let tr' = resType $ infoType $ getInfo lam2
            let sr' = snd $ infoSize $ getInfo lam2
            let ix = mkVar (compileTypeRep t sz) v
            len' <- mkLength len (infoType $ getInfo len) sz
            tmp  <- freshVar "seq" tr' sr'
            (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s (StructField tmp "member2") $ compileProg tmp step
            tellProg [initArray (AddrOf loc) len']
            compileProg (StructField tmp "member2") st
            tellProg [toProg $ Block ds $
                      for (lName ix) len' 1 $
                                    toBlock $ Sequence (body ++
                                         [copyProg (ArrayElem (AddrOf loc) ix) [StructField tmp "member1"]
                                         ])]

    -- loc = parallel l f ++ parallel l g ==> for l (\i -> loc[i] = f i; loc[i+l] = g i)
    compileProgSym (C' Append) _ loc ((arr1 :$ l1 :$ (lam1 :$ body1)) :* (arr2 :$ l2 :$ (lam2 :$ body2)) :* Nil)
        | Just (C' Parallel) <- prjF arr1
        , Just (C' Parallel) <- prjF arr2
        , Just (SubConstr2 (Lambda v1)) <- prjLambda lam1
        , Just (SubConstr2 (Lambda v2)) <- prjLambda lam2
        , alphaEq l1 l2
        = do
            let t   = argType $ infoType $ getInfo lam1
                sz  = fst $ infoSize $ getInfo lam1
                ix1 = mkVar (compileTypeRep t sz) v1
                ix2 = mkVar (compileTypeRep t sz) v2
            len <- mkLength l1 (infoType $ getInfo l1) sz
            (_, Block ds1 (Sequence b1)) <- confiscateBlock $ withAlias v1 ix1 $ compileProg (ArrayElem (AddrOf loc) ix1) body1
            (_, Block ds2 (Sequence b2)) <- confiscateBlock $ withAlias v2 ix1 $ compileProg (ArrayElem (AddrOf loc) ix2) body2
            tellProg [initArray (AddrOf loc) len]
            assign ix2 len
            tellProg [for (lName ix1) len 1 (Block (ds1++ds2) (Sequence $ b1 ++ b2 ++ [copyProg ix2 [(binop (Rep.NumType Unsigned S32) "+" ix2 (litI32 1))]]))]

    compileProgSym (C' Append) _ loc (a :* b :* Nil) = do
        a' <- compileExpr a
        b' <- compileExpr b
        tellProg [copyProg loc [AddrOf a',AddrOf b']]
        -- TODO: Optimize by writing to directly to 'loc' instead of 'a'' and 'b''!
        --       But take care of array initialization:
        --       compiling 'a' and 'b' might do initialization itself...

    compileProgSym (C' SetIx) _ loc (arr :* i :* a :* Nil) = do
        compileProg loc arr
        i' <- compileExpr i
        compileProg (ArrayElem (AddrOf loc) i') a

    compileProgSym (C' SetLength) _ loc (len :* arr :* Nil) = do
        len' <- compileExpr len
        compileProg loc arr
        tellProg [setLength loc len']
      -- TODO Optimize by using copyProgLen (compare to 0.4)

    compileProgSym a info loc args = compileExprLoc a info loc args

    compileExprSym (C' GetLength) info (a :* Nil) = do
        aExpr <- compileExpr a
        return $ fun (compileTypeRep (infoType info) (infoSize info)) "getLength" [AddrOf aExpr]

    compileExprSym (C' GetIx) _ (arr :* i :* Nil) = do
        a' <- compileExpr arr
        i' <- compileExpr i
        return $ ArrayElem (AddrOf a') i'

    compileExprSym a info args = compileProgFresh a info args

