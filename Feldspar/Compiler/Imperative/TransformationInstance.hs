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

{-# LANGUAGE UndecidableInstances, OverlappingInstances #-}

module Feldspar.Compiler.Imperative.TransformationInstance where


import Feldspar.Transformation.Framework
import Feldspar.Compiler.Imperative.Representation

-- =========================================
-- == Classes for the plugin architecture ==
-- =========================================

-- class to simplify contexts
class (Transformation t, Convert (Label (From t) s) (Label (To t) s), Default (Label (To t) s)) => Conversion t s

instance (Transformation t, Convert (Label (From t) s) (Label (To t) s), Default (Label (To t) s)) => Conversion t s

-- ====================
-- == Transformation ==
-- ====================

instance (Transformable1 t [] Entity, Conversion t Module)
    => DefaultTransformable t Module where
        defaultTransform t s d (Module m inf) = Result (Module (result1 tr) $ convert inf) (state1 tr) (up1 tr) where
            tr = transform1 t s d m

instance (Transformable1 t [] StructMember, Transformable1 t [] Variable, Transformable t Block, Transformable t Declaration, Conversion t Entity, Conversion t Struct, Conversion t ProcDef, Conversion t ProcDecl)
    => DefaultTransformable t Entity where
        defaultTransform t s d (StructDef n m inf1 inf2) =
            Result (StructDef n (result1 tr) (convert inf1) $ convert inf2) (state1 tr) (up1 tr) where
                tr = transform1 t s d m
        defaultTransform t s d (TypeDef typ n inf) =
            Result (TypeDef typ n (convert inf)) s def
        defaultTransform t s d (ProcDef n i o p inf1 inf2) =
            Result (ProcDef n (result1 tr1) (result1 tr2) (result tr3) (convert inf1) $ convert inf2)
                   (state tr3) (foldl combine (up1 tr1) [up1 tr2, up tr3]) where
                        tr1 = transform1 t s d i
                        tr2 = transform1 t (state1 tr1) d o
                        tr3 = transform t (state1 tr2) d p
        defaultTransform t s d (ProcDecl name inp outp inf1 inf2) =
            Result (ProcDecl name (result1 tr1) (result1 tr2) (convert inf1) (convert inf2))
                   (state1 tr2) (foldl1 combine [up1 tr1, up1 tr2]) where
                tr1 = transform1 t s d inp
                tr2 = transform1 t (state1 tr1) d outp

instance (Conversion t StructMember, Default (Up t))
    => DefaultTransformable t StructMember where
        defaultTransform t s d (StructMember n typ inf) = Result (StructMember n typ $ convert inf) s def

instance (Transformable1 t [] Declaration, Transformable t Program, Conversion t Block)
    => DefaultTransformable t Block where
        defaultTransform t s d (Block l b inf) = Result (Block (result1 tr1) (result tr2) $ convert inf) (state tr2) (combine (up1 tr1) (up tr2)) where
            tr1 = transform1 t s d l
            tr2 = transform t (state1 tr1) d b

instance (Transformable1 t [] Program, Transformable t Expression, Transformable1 t [] ActualParameter, Transformable t Block, Transformable t Variable, Conversion t Program, Conversion t Empty, Conversion t Comment, Conversion t Assign, Conversion t ProcedureCall, Conversion t Spawn, Conversion t Run, Conversion t Sequence, Conversion t Branch, Conversion t SeqLoop, Conversion t ParLoop, Default (Up t))
    => DefaultTransformable t Program where
        defaultTransform t s d (Empty inf1 inf2) = Result (Empty (convert inf1) $ convert inf2) s def
        defaultTransform t s d (Comment b c inf1 inf2) = Result (Comment b c (convert inf1) $ convert inf2) s def
        defaultTransform t s d (Assign l r inf1 inf2) = Result (Assign (result tr1) (result tr2) (convert inf1) $ convert inf2) (state tr2) (combine (up tr1) (up tr2)) where
            tr1 = transform t s d l
            tr2 = transform t (state tr1) d r
        defaultTransform t s d (ProcedureCall f par inf1 inf2) = Result (ProcedureCall f (result1 tr) (convert inf1) $ convert inf2) (state1 tr) (up1 tr) where
            tr = transform1 t s d par
        defaultTransform t s d (Sequence p inf1 inf2) = Result (Sequence (result1 tr) (convert inf1) $ convert inf2) (state1 tr) (up1 tr) where
            tr = transform1 t s d p
        defaultTransform t s d (Branch e p1 p2 inf1 inf2) = Result (Branch (result tr1) (result tr2) (result tr3) (convert inf1) $ convert inf2) (state tr3) (foldl combine (up tr1) [up tr2, up tr3]) where
            tr1 = transform t s d e
            tr2 = transform t (state tr1) d p1
            tr3 = transform t (state tr2) d p2
        defaultTransform t s d (SeqLoop v c p inf1 inf2) = Result (SeqLoop (result tr1) (result tr2) (result tr3) (convert inf1) $ convert inf2) (state tr3) (foldl combine (up tr1) [up tr2, up tr3]) where
            tr1 = transform t s d v
            tr2 = transform t (state tr1) d c
            tr3 = transform t (state tr2) d p
        defaultTransform t s d (ParLoop v b i p inf1 inf2) = Result (ParLoop (result tr1) (result tr2) i (result tr3) (convert inf1) $ convert inf2) (state tr3) (foldl combine (up tr1) [up tr2, up tr3]) where
            tr1 = transform t s d v
            tr2 = transform t (state tr1) d b
            tr3 = transform t (state tr2) d p
        defaultTransform t s d (BlockProgram b inf) = Result (BlockProgram (result tr) $ convert inf) (state tr) (up tr) where
            tr = transform t s d b

instance (Transformable t Expression, Conversion t ActualParameter)
    => DefaultTransformable t ActualParameter where
        defaultTransform t s d (In p inf) = Result (In (result tr) $ convert inf) (state tr) (up tr) where
            tr = transform t s d p
        defaultTransform t s d (Out p inf) = Result (Out (result tr) $ convert inf) (state tr) (up tr) where
            tr = transform t s d p
        defaultTransform t s d (TypeParameter p r inf) = Result (TypeParameter p r $ convert inf) s def
        defaultTransform t s d (FunParameter n b inf) = Result (FunParameter n b $ convert inf) s def

instance (Transformable t Variable, Transformable1 t Maybe Expression, Conversion t Declaration)
    => DefaultTransformable t Declaration where
        defaultTransform t s d (Declaration v i inf) = Result (Declaration (result tr1) (result1 tr2) $ convert inf) (state1 tr2) (combine (up tr1) (up1 tr2)) where
            tr1 = transform t s d v
            tr2 = transform1 t (state tr1) d i

instance (Transformable t Expression, Transformable t Variable, Transformable t Constant, Transformable1 t [] Expression, Conversion t Expression, Conversion t FunctionCall, Conversion t ArrayElem, Conversion t StructField, Conversion t SizeOf, Conversion t Cast, Default (Up t))
    => DefaultTransformable t Expression where
        defaultTransform t s d (VarExpr v inf) = Result (VarExpr (result tr) $ convert inf) (state tr) (up tr) where
            tr = transform t s d v
        defaultTransform t s d (ArrayElem a i inf1 inf2) = Result (ArrayElem (result tr1) (result tr2) (convert inf1) (convert inf2)) (state tr2) (combine (up tr1) (up tr2)) where
            tr1 = transform t s d a
            tr2 = transform t (state tr1) d i
        defaultTransform t s d (StructField l n inf1 inf2) = Result (StructField (result tr) n (convert inf1) $ convert inf2) (state tr) (up tr) where
            tr = transform t s d l
        defaultTransform t s d (ConstExpr c inf) = Result (ConstExpr (result tr) $ convert inf) (state tr) (up tr) where
            tr = transform t s d c
        defaultTransform t s d (FunctionCall f par inf1 inf2) =
            Result (FunctionCall f (result1 tr) (convert inf1) $ convert inf2) (state1 tr) (up1 tr) where
                tr = transform1 t s d par
        defaultTransform t s d (Cast typ exp inf1 inf2) = Result (Cast typ (result tr) (convert inf1) $ convert inf2) (state tr) (up tr) where
            tr = transform t s d exp
        defaultTransform t s d (SizeOf par inf1 inf2) = case par of
            Left typ -> Result (SizeOf (Left typ) (convert inf1) $ convert inf2) s def
            Right exp -> Result (SizeOf (Right $ result tr) (convert inf1) $ convert inf2) (state tr) (up tr) where
                tr = transform t s d exp

instance (Transformable t Constant, Transformable1 t [] Constant, Conversion t Constant, Conversion t IntConst, Conversion t FloatConst, Conversion t BoolConst, Conversion t ArrayConst, Conversion t ComplexConst, Default (Up t))
    => DefaultTransformable t Constant where
        defaultTransform t s d (IntConst c typ inf1 inf2) = Result (IntConst c typ (convert inf1) $ convert inf2) s def
        defaultTransform t s d (FloatConst c inf1 inf2) = Result (FloatConst c (convert inf1) $ convert inf2) s def
        defaultTransform t s d (BoolConst c inf1 inf2) = Result (BoolConst c (convert inf1) $ convert inf2) s def
        -- defaultTransform t s d (ArrayConst c inf1 inf2) = Result (ArrayConst (result1 tr) (convert inf1) $ convert inf2) (state1 tr) (up1 tr) where
            -- tr = transform1 t s d c
        defaultTransform t s d (ComplexConst re im inf1 inf2) = Result (ComplexConst (result tr1) (result tr2) (convert inf1) $ convert inf2) (state tr2) (combine (up tr1) $ up tr2) where
            tr1 = transform t s d re
            tr2 = transform t (state tr1) d im

instance (Conversion t Variable, Default (Up t))
    => DefaultTransformable t Variable where
        defaultTransform t s d (Variable name typ role inf) = Result (Variable name typ role $ convert inf) s def


instance (Transformable t a, Default (Up t), Combine (Up t), Transformation t)
    => DefaultTransformable1 t [] a where
        defaultTransform1 t s d [] = Result1 [] s def
        defaultTransform1 t s d [x] = Result1 [result tr] (state tr) (up tr) where
            tr  = transform t s d x
        defaultTransform1 t s d (x:xs) = Result1 ((result tr1):(result1 tr2)) (state1 tr2) (combine (up tr1) (up1 tr2)) where
            tr1 = transform t s d x
            tr2 = transform1 t (state tr1) d xs

instance (Transformable t a, Default (Up t), Transformation t)
    => DefaultTransformable1 t Maybe a where
        defaultTransform1 t s d Nothing = Result1 Nothing s def
        defaultTransform1 t s d (Just x) = Result1 (Just $ result tr) (state tr) (up tr) where
            tr = transform t s d x
