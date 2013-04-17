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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.TransformationInstance where


import Feldspar.Transformation.Framework
import Feldspar.Compiler.Imperative.Representation

-- =========================================
-- == Classes for the plugin architecture ==
-- =========================================

-- ====================
-- == Transformation ==
-- ====================

instance (Transformable1 t [] Entity)
    => DefaultTransformable t Module where
        defaultTransform t s d (Module m) = Result (Module (result1 tr)) (state1 tr) (up1 tr) where
            tr = transform1 t s d m

instance (Transformable1 t [] StructMember, Transformable1 t [] Variable, Transformable t Block, Transformable t Declaration, Transformable t Constant, Combine (Up t), Default (Up t))
    => DefaultTransformable t Entity where
        defaultTransform t s d (StructDef n m) =
            Result (StructDef n (result1 tr)) (state1 tr) (up1 tr) where
                tr = transform1 t s d m
        defaultTransform _ s _ (TypeDef typ n) =
            Result (TypeDef typ n) s def
        defaultTransform t s d (ProcDef n i o p) =
            Result (ProcDef n (result1 tr1) (result1 tr2) (result tr3))
                   (state tr3) (foldl combine (up1 tr1) [up1 tr2, up tr3]) where
                        tr1 = transform1 t s d i
                        tr2 = transform1 t (state1 tr1) d o
                        tr3 = transform t (state1 tr2) d p
        defaultTransform t s d (ProcDecl name inp outp) =
            Result (ProcDecl name (result1 tr1) (result1 tr2))
                   (state1 tr2) (foldl1 combine [up1 tr1, up1 tr2]) where
                tr1 = transform1 t s d inp
                tr2 = transform1 t (state1 tr1) d outp
        defaultTransform t s d (ValueDef var val) =
            Result (ValueDef (result tr1) (result tr2)) (state tr2) (combine (up tr1) (up tr2)) where
                tr1 = transform t s d var
                tr2 = transform t (state tr1) d val

instance (Default (Up t))
    => DefaultTransformable t StructMember where
        defaultTransform _ s _ (StructMember n typ) = Result (StructMember n typ) s def

instance (Transformable1 t [] Declaration, Transformable t Program, Combine (Up t))
    => DefaultTransformable t Block where
        defaultTransform t s d (Block l b) = Result (Block (result1 tr1) (result tr2)) (state tr2) (combine (up1 tr1) (up tr2)) where
            tr1 = transform1 t s d l
            tr2 = transform t (state1 tr1) d b

instance (Transformable1 t [] Program, Transformable t Expression, Transformable1 t [] ActualParameter, Transformable t Block, Transformable t Variable, Combine (Up t), Default (Up t))
    => DefaultTransformable t Program where
        defaultTransform _ s _ Empty = Result Empty s def
        defaultTransform _ s _ (Comment b c) = Result (Comment b c) s def
        defaultTransform t s d (Assign l r) = Result (Assign (result tr1) (result tr2)) (state tr2) (combine (up tr1) (up tr2)) where
            tr1 = transform t s d l
            tr2 = transform t (state tr1) d r
        defaultTransform t s d (ProcedureCall f par) = Result (ProcedureCall f (result1 tr)) (state1 tr) (up1 tr) where
            tr = transform1 t s d par
        defaultTransform t s d (Sequence p) = Result (Sequence (result1 tr)) (state1 tr) (up1 tr) where
            tr = transform1 t s d p
        defaultTransform t s d (Branch e p1 p2) = Result (Branch (result tr1) (result tr2) (result tr3)) (state tr3) (foldl combine (up tr1) [up tr2, up tr3]) where
            tr1 = transform t s d e
            tr2 = transform t (state tr1) d p1
            tr3 = transform t (state tr2) d p2
        defaultTransform t s d (Switch scrut alts) = error "TODO: defaultTransform for switch" where -- Result (Switch (result tr1) (result def':map result alts') (convert inf1) $ convert inf2) (state (last alts')) (foldl combine (up tr1) (up def':map up alts')) where
            tr1 = transform t s d scrut
{-            def' = transform t (state tr1) d (head alts)
            alts' = go def' [] (tail alts)
            go prev acc [] = reverse (prev:acc)
            go prev acc (h:t) = go (transform t (state prev) d h) (prev:acc) t
-}
        defaultTransform t s d (SeqLoop v c p) = Result (SeqLoop (result tr1) (result tr2) (result tr3)) (state tr3) (foldl combine (up tr1) [up tr2, up tr3]) where
            tr1 = transform t s d v
            tr2 = transform t (state tr1) d c
            tr3 = transform t (state tr2) d p
        defaultTransform t s d (ParLoop pp v b i p) = Result (ParLoop pp (result tr1) (result tr2) i (result tr3)) (state tr3) (foldl combine (up tr1) [up tr2, up tr3]) where
            tr1 = transform t s d v
            tr2 = transform t (state tr1) d b
            tr3 = transform t (state tr2) d p
        defaultTransform t s d (BlockProgram b) = Result (BlockProgram (result tr)) (state tr) (up tr) where
            tr = transform t s d b

instance (Transformable t Expression, Default (Up t))
    => DefaultTransformable t ActualParameter where
        defaultTransform t s d (ValueParameter p) = Result (ValueParameter (result tr)) (state tr) (up tr) where
            tr = transform t s d p
        defaultTransform _ s _ (TypeParameter p) = Result (TypeParameter p) s def
        defaultTransform _ s _ (FunParameter n) = Result (FunParameter n) s def

instance (Transformable t Variable, Transformable1 t Maybe Expression, Combine (Up t))
    => DefaultTransformable t Declaration where
        defaultTransform t s d (Declaration v i) = Result (Declaration (result tr1) (result1 tr2)) (state1 tr2) (combine (up tr1) (up1 tr2)) where
            tr1 = transform t s d v
            tr2 = transform1 t (state tr1) d i

instance (Transformable t Expression, Transformable t Variable, Transformable t Constant, Transformable1 t [] Expression, Combine (Up t), Default (Up t))
    => DefaultTransformable t Expression where
        defaultTransform t s d (VarExpr v) = Result (VarExpr (result tr)) (state tr) (up tr) where
            tr = transform t s d v
        defaultTransform t s d (ArrayElem a i) = Result (ArrayElem (result tr1) (result tr2)) (state tr2) (combine (up tr1) (up tr2)) where
            tr1 = transform t s d a
            tr2 = transform t (state tr1) d i
        defaultTransform t s d (StructField l n) = Result (StructField (result tr) n) (state tr) (up tr) where
            tr = transform t s d l
        defaultTransform t s d (ConstExpr c) = Result (ConstExpr (result tr)) (state tr) (up tr) where
            tr = transform t s d c
        defaultTransform t s d (FunctionCall f par) =
            Result (FunctionCall f (result1 tr)) (state1 tr) (up1 tr) where
                tr = transform1 t s d par
        defaultTransform t s d (Cast typ ex) = Result (Cast typ (result tr)) (state tr) (up tr) where
            tr = transform t s d ex
        defaultTransform t s d (AddrOf ex) = Result (AddrOf (result tr)) (state tr) (up tr) where
            tr = transform t s d ex
        defaultTransform t s d (SizeOf par) = case par of
            Left typ -> Result (SizeOf (Left typ)) s def
            Right ex -> Result (SizeOf (Right $ result tr)) (state tr) (up tr) where
                tr = transform t s d ex

instance (Transformable t Constant, Transformable1 t [] Constant, Combine (Up t), Default (Up t))
    => DefaultTransformable t Constant where
        defaultTransform _ s _ (IntConst c typ) = Result (IntConst c typ) s def
        defaultTransform _ s _ (FloatConst c) = Result (FloatConst c) s def
        defaultTransform _ s _ (BoolConst c) = Result (BoolConst c) s def
        defaultTransform t s d (ArrayConst c) = Result (ArrayConst (result1 tr)) (state1 tr) (up1 tr) where
          tr = transform1 t s d c
        defaultTransform t s d (ComplexConst re im) = Result (ComplexConst (result tr1) (result tr2)) (state tr2) (combine (up tr1) $ up tr2) where
            tr1 = transform t s d re
            tr2 = transform t (state tr1) d im

instance (Default (Up t))
    => DefaultTransformable t Variable where
        defaultTransform _ s _ (Variable name typ) = Result (Variable name typ) s def


instance (Transformable t a, Default (Up t), Combine (Up t), Transformation t)
    => DefaultTransformable1 t [] a where
        defaultTransform1 _ s _ [] = Result1 [] s def
        defaultTransform1 t s d [x] = Result1 [result tr] (state tr) (up tr) where
            tr  = transform t s d x
        defaultTransform1 t s d (x:xs) = Result1 (result tr1 : result1 tr2) (state1 tr2) (combine (up tr1) (up1 tr2)) where
            tr1 = transform t s d x
            tr2 = transform1 t (state tr1) d xs

instance (Transformable t a, Default (Up t), Transformation t)
    => DefaultTransformable1 t Maybe a where
        defaultTransform1 _ s _ Nothing = Result1 Nothing s def
        defaultTransform1 t s d (Just x) = Result1 (Just $ result tr) (state tr) (up tr) where
            tr = transform t s d x
