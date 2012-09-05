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

{-# LANGUAGE EmptyDataDecls, TypeFamilies #-}

module Feldspar.Compiler.Backend.C.Plugin.TypeCorrector where

import Data.List
import qualified Data.Map as Map
import Feldspar.Transformation
import Feldspar.Compiler.Backend.C.CodeGeneration
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Error

-- ===========================================================================
--  == Type corrector plugin
-- ===========================================================================
-- TODO: IS THIS STILL NEEDED? 

typeCorrectorError = handleError "PluginArch/TypeCorrector" InternalError

type TypeCatalog = Map.Map String Type

instance Default TypeCatalog where
    def = Map.empty

-- first collect types of global variables
data GlobalCollector = GlobalCollector
instance Transformation GlobalCollector where
    type From GlobalCollector = ()
    type To GlobalCollector = ()
    type Down GlobalCollector = Bool -- variable is global
    type Up GlobalCollector = ()
    type State GlobalCollector = TypeCatalog


instance Transformable GlobalCollector Entity where
        transform t s d p = defaultTransform t s False p

instance Transformable GlobalCollector Variable where
        transform t s d v@(Variable name typ role ()) = Result v s' () where
            s'
             | d            = Map.insert name typ s
             | otherwise    = s

data TypeCheckDown = TypeCheckDown
    { globals       :: TypeCatalog
    , inDeclaration :: Bool
    }

inDecl d b = d {inDeclaration = b}

instance Default [String] where
    def = []
instance Combine [String] where
    combine = (++)

-- get globals as state collect types of variables in a procedure, then corrert types
data TypeCheck = TypeCheck
instance Transformation TypeCheck where
    type From TypeCheck = ()
    type To TypeCheck = ()
    type Down TypeCheck = TypeCheckDown     -- globals variable's type, in a declaration
    type Up TypeCheck = [String]                -- errors
    type State TypeCheck = TypeCatalog          -- local variable's types
    
instance Transformable TypeCheck Entity where
        transform t s d p@(ProcDef _ _ _ _ _ _) = defaultTransform t s' d' p where
            s' = def                       -- start with an empty local variable type catalog
            d' = inDecl d True             --input parameters are declarations (block will correct where it isn't good)
        transform t s d p = Result p s def -- just definitions, not implementation, not need check/correct
                
instance Transformable TypeCheck Block where
    transform t s d b = tr
        { result = (result tr)
            { blockBody = mkSeq (err (up tr)) $ blockBody $ result tr
            }
        , state = s -- forget locals
        , up = []
        }
        where
            tr = defaultTransform t s (inDecl d False) b   -- we are'n in declaration (correct the procedure's change)
            mkSeq (Empty _ _) x = x
            mkSeq p (Sequence ps _ _) = Sequence (p:ps) () ()
            mkSeq p p2 = Sequence [p, p2] () ()
            err [] = Empty () ()
            err x  = Comment True (listprint id "\n " $ uniq x) () ()
            uniq [] = []
            uniq (x:xs) = x:(uniq $ filter (/= x) xs)

instance Transformable TypeCheck Declaration where
        transform t s d (Declaration v i inf) = Result (Declaration (result tr1) (result1 tr2) $ convert inf) (state1 tr2) (combine (up tr1) (up1 tr2)) where
            tr1 = transform t s (inDecl d True) v
            tr2 = transform1 t (state tr1) d i

instance Transformable TypeCheck Program where
        transform t s d (ParLoop v b i p inf1 inf2) = Result (ParLoop (result tr1) (result tr2) i (result tr3) (convert inf1) $ convert inf2) s' (foldl combine (up tr1) [up tr2, up tr3]) where
            tr1 = transform t s (inDecl d True) v     -- loop variable is an undeclared local
            tr2 = transform t (state tr1) d b
            tr3 = transform t (state tr2) d p
            s' = s                                    -- forget loop variable (no other new var can be here, other news deleted at block)
        transform t s d p = defaultTransform t s d p

instance Transformable TypeCheck Variable where
        transform t s d v@(Variable name typ role ()) 
            | inDeclaration d = Result v (Map.insert name typ s) def
            | otherwise       = Result v s u' where
                u' = case Map.lookup name allVar of
                    Just typ2
                        | typ == typ2 -> []
                        | otherwise   -> ["Inconsistent types: " ++ name ++ " (actual type: " ++show typ ++ ", declared type: " ++ show typ2 ++ ")"]
                    Nothing -> ["Undeclared variable: " ++ name]
                allVar :: TypeCatalog
                allVar = Map.unionWith (\global local -> local) (globals d) s

data TypeCorrector = TypeCorrector
instance Transformation TypeCorrector where
    type From TypeCorrector = ()
    type To TypeCorrector = ()
    type Down TypeCorrector = TypeCatalog     -- global variable's type, in a declaration
    type Up TypeCorrector = ()
    type State TypeCorrector = TypeCatalog    -- local variable's types

instance Transformable TypeCorrector Entity where
    transform t s d p@(ProcDef n i o _ _ _) = defaultTransform t (state tr) d p where
        tr = defaultTransform t def d p -- start with an empty local variable type catalog
    transform t s d p = Result p s def -- just definitions, not implementation, not need check/correct

instance Transformable TypeCorrector Variable where
    transform t locals globals v@(Variable name typ role ()) = Result v' (Map.insert name typ' locals) def where
        v' = v {varType = typ'}
        typ' = case Map.lookup name allVar of
            Just typ2
                | typ == typ2 -> typ2
                | otherwise   -> select typ typ2
            Nothing -> typ
        allVar = Map.unionWith (\global local -> local) globals locals

select act decl
    | ok        = typ
    | otherwise = decl
    where
        (ok,typ) = select' act decl
        select' (ComplexType t1) (ComplexType t2) = (o, ComplexType t) where
            (o,t) = select' t1 t2
        select' (ArrayType l1 t1) (ArrayType l2 t2) = (o && o2, ArrayType l t) where
            (o,t) = select' t1 t2
            (o2,l) = select'' l1 l2
            select'' UndefinedLen x = (True, x)
            select'' x UndefinedLen = (True, x)
            select'' (LiteralLen a) (LiteralLen b) = (a==b, (LiteralLen b))
        select' (StructType t1) (StructType t2) = (o, StructType t) where
            (o,t) = select'' t1 t2
            select'' [] [] = (True, [])
            select'' [] _ = (False, undefined)
            select'' _ [] = (False, undefined)
            select'' ((a,st1):ts1) ((b,st2):ts2) = ( a==b && oo && ooo, (a,tt):tts) where
                (oo,tt) = select' st1 st2
                (ooo,tts) = select'' ts1 ts2
        select' t1 t2
            | t1 == t2 = (True, t1)
            | otherwise = (False, undefined)

instance Plugin TypeCorrector where
    type ExternalInfo TypeCorrector = Bool
    executePlugin TypeCorrector showErr procedure = 
        result $ transform TypeCorrector def globals x where
            globals = {- Map.insert defaultArraySizeConstantName (NumType Unsigned S32) $ -} state $ transform GlobalCollector def False procedure
            x 
                | showErr = result $ transform TypeCheck def (TypeCheckDown globals False) procedure
                | otherwise = procedure
