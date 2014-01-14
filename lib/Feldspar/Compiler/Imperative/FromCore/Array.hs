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


import Data.Typeable

import Control.Applicative

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
                                                    Signedness(..), typeof)
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
            (_, b) <- confiscateBlock $ compileProg (ArrayElem <$> loc <*> pure ix) ixf
            tellProg [initArray loc len']
            tellProg [for True (lName ix) len' (litI32 1) b]


    compileProgSym (C' Sequential) _ loc (len :* init' :* (lam1 :$ lt1) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam1
        , (bs1, lam2 :$ l)             <- collectLetBinders lt1
        , Just (SubConstr2 (Lambda s)) <- prjLambda lam2
        , (bs, tup :$ a :$ b)          <- collectLetBinders l
        , Just (C' Tup2)               <- prjF tup
        , not $ null bs
        , (e, ASTB step)               <- last bs
        , Just (C' (Variable t1))      <- prjF a
        , Just (C' (Variable t2))      <- prjF b
        , t1 == e
        , t2 == e
        = do
            blocks <- mapM (confiscateBlock . compileBind) (bs1 ++ init bs)
            let tix = argType $ infoType $ getInfo lam1
                six = fst $ infoSize $ getInfo lam1
                tst = infoType $ getInfo step
                sst = infoSize $ getInfo step
                (dss, lets) = unzip $ map (\(_, Block ds (Sequence body)) -> (ds, body)) blocks
            let ix = mkVar (compileTypeRep tix six) v
            len' <- mkLength len (infoType $ getInfo len) six
            st1 <- freshVar "st" tst sst
            let st = mkRef (compileTypeRep tst sst) s
            declareAlias st
            (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s st $ compileProg (ArrayElem <$> loc <*> pure ix) step
            withAlias s st $ compileProg (Just st1) init'
            tellProg [ Assign st st1
                     , initArray loc len']
            tellProg [toProg $ Block (concat dss ++ ds) $
                      for False (lName ix) len' (litI32 1) $
                                    toBlock $ Sequence (concat lets ++ body ++ maybe [] (\arr -> [Assign st (ArrayElem arr ix)]) loc)]

    compileProgSym (C' Sequential) _ loc (len :* st :* (lam1 :$ lt1) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam1
        , (bs1, lam2 :$ step)          <- collectLetBinders lt1
        , Just (SubConstr2 (Lambda s)) <- prjLambda lam2
        = do
            blocks <- mapM (confiscateBlock . compileBind) bs1
            let t = argType $ infoType $ getInfo lam1
            let sz = fst $ infoSize $ getInfo lam1
            let tr' = resType $ infoType $ getInfo lam2
            let sr' = snd $ infoSize $ getInfo lam2
            let ix = mkVar (compileTypeRep t sz) v
                (dss, lets) = unzip $ map (\(_, Block ds (Sequence body)) -> (ds, body)) blocks
            len' <- mkLength len (infoType $ getInfo len) sz
            tmp  <- freshVar "seq" tr' sr'
            (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s (StructField tmp "member2") $ compileProg (Just tmp) step
            tellProg [initArray loc len']
            compileProg (Just $ StructField tmp "member2") st
            tellProg [toProg $ Block (concat dss ++ ds) $
                      for False (lName ix) len' (litI32 1) $
                                    toBlock $ Sequence (concat lets ++ body ++
                                         [copyProg (ArrayElem <$> loc <*> pure ix) [StructField tmp "member1"]
                                         ])]

    -- TODO: This optimization contains a bug
    -- The result array is initialized to length l, when it should be 2*l
    -- It is easy enough to fix by multiplying the length with 2 like this:
    --
    -- > tellprog [initArray loc $ binop (Rep.NumType Unsigned S32) "*" len (litI32 2)]
    --
    -- However, in many cases the resulting length expression could be
    -- optimized further if the calculation had been performed in the core
    -- language instead of the imperative langauge.
    --
    --
    -- -- loc = parallel l f ++ parallel l g ==> for l (\i -> loc[i] = f i; loc[i+l] = g i)
    -- compileProgSym (C' Append) _ loc ((arr1 :$ l1 :$ (lam1 :$ body1)) :* (arr2 :$ l2 :$ (lam2 :$ body2)) :* Nil)
    --     | Just (C' Parallel) <- prjF arr1
    --     , Just (C' Parallel) <- prjF arr2
    --     , Just (SubConstr2 (Lambda v1)) <- prjLambda lam1
    --     , Just (SubConstr2 (Lambda v2)) <- prjLambda lam2
    --     , alphaEq l1 l2
    --     = do
    --         let t   = argType $ infoType $ getInfo lam1
    --             sz  = fst $ infoSize $ getInfo lam1
    --             ix1 = mkVar (compileTypeRep t sz) v1
    --             ix2 = mkVar (compileTypeRep t sz) v2
    --         len <- mkLength l1 (infoType $ getInfo l1) sz
    --         (_, Block ds1 (Sequence b1)) <- confiscateBlock $ withAlias v1 ix1 $ compileProg (ArrayElem <$> loc <*> pure ix1) body1
    --         (_, Block ds2 (Sequence b2)) <- confiscateBlock $ withAlias v2 ix1 $ compileProg (ArrayElem <$> loc <*> pure ix2) body2
    --         tellProg [initArray loc len]
    --         assign (Just ix2) len
    --         tellProg [for True (lName ix1) len (litI32 1) (Block (ds1++ds2) (Sequence $ b1 ++ b2 ++ [copyProg (Just ix2) [binop (Rep.NumType Unsigned S32) "+" ix2 (litI32 1)]]))]

    compileProgSym (C' Append) _ loc (a :* b :* Nil) = do
        a' <- compileExpr a
        b' <- compileExpr b
        tellProg [copyProg loc [a', b']]
        -- TODO: Optimize by writing to directly to 'loc' instead of 'a'' and 'b''!
        --       But take care of array initialization:
        --       compiling 'a' and 'b' might do initialization itself...

    compileProgSym (C' SetIx) _ loc (arr :* i :* a :* Nil) = do
        compileProg loc arr
        i' <- compileExpr i
        compileProg (ArrayElem <$> loc <*> pure i') a

    compileProgSym (C' GetIx) _ (Just loc) (arr :* i :* Nil) = do
        a' <- compileExpr arr
        i' <- compileExpr i
        let el = ArrayElem a' i'
        tellProg $ if isArray $ typeof el
                     then [Assign loc el]
                     else [copyProg (Just loc) [el]]

    compileProgSym (C' SetLength) _ loc (len :* arr :* Nil) = do
        len' <- compileExpr len
        compileProg loc arr
        tellProg [setLength loc len']
      -- TODO Optimize by using copyProgLen (compare to 0.4)

    compileProgSym a info loc args = compileExprLoc a info loc args

    compileExprSym (C' GetLength) _ (a :* Nil) = do
        aExpr <- compileExpr a
        return $ arrayLength aExpr

    compileExprSym (C' GetIx) _ (arr :* i :* Nil) = do
        a' <- compileExpr arr
        i' <- compileExpr i
        return $ ArrayElem a' i'

    compileExprSym a info args = compileProgFresh a info args

