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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Backend.C.Plugin.TypeCorrector where

import qualified Data.Map as Map
import Feldspar.Transformation
import Feldspar.Compiler.Backend.C.CodeGeneration
import Feldspar.Compiler.Error

import Feldspar.Range

-- ===========================================================================
--  == Type corrector plugin
-- ===========================================================================
-- TODO: IS THIS STILL NEEDED? 

typeCorrectorError :: String -> a
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
        transform t s _ = defaultTransform t s False

instance Transformable GlobalCollector Variable where
        transform _ s d v@Variable{..} = Result v s' () where
            s'
             | d         = Map.insert varName varType s
             | otherwise = s

data TypeCheckDown = TypeCheckDown
    { globals       :: TypeCatalog
    , inDeclaration :: Bool
    }

inDecl :: TypeCheckDown -> Bool -> TypeCheckDown
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
        transform t _ d p@ProcDef{} = defaultTransform t s' d' p where
            s' = def                       -- start with an empty local variable type catalog
            d' = inDecl d True             --input parameters are declarations (block will correct where it isn't good)
        transform _ s _ p = Result p s def -- just definitions, not implementation, not need check/correct
                
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
            mkSeq Empty x = x
            mkSeq p (Sequence ps) = Sequence (p:ps)
            mkSeq p p2 = Sequence [p, p2]
            err [] = Empty
            err x  = Comment True (listprint id "\n " $ uniq x)
            uniq [] = []
            uniq (x:xs) = x : uniq (filter (/= x) xs)

instance Transformable TypeCheck Declaration where
        transform t s d (Declaration v i) = Result (Declaration (result tr1) (result1 tr2)) (state1 tr2) (combine (up tr1) (up1 tr2)) where
            tr1 = transform t s (inDecl d True) v
            tr2 = transform1 t (state tr1) d i

instance Transformable TypeCheck Program where
        transform t s d (ParLoop v b i p) = Result (ParLoop (result tr1) (result tr2) i (result tr3)) s' (foldl combine (up tr1) [up tr2, up tr3]) where
            tr1 = transform t s (inDecl d True) v     -- loop variable is an undeclared local
            tr2 = transform t (state tr1) d b
            tr3 = transform t (state tr2) d p
            s' = s                                    -- forget loop variable (no other new var can be here, other news deleted at block)
        transform t s d p = defaultTransform t s d p

instance Transformable TypeCheck Variable where
        transform _ s d v@Variable{..}
            | inDeclaration d = Result v (Map.insert varName varType s) def
            | otherwise       = Result v s u' where
                u' = case Map.lookup varName allVar of
                    Just typ
                        | varType == typ -> []
                        | otherwise   -> ["Inconsistent types: " ++ varName ++ " (actual type: " ++ show varType ++ ", declared type: " ++ show typ ++ ")"]
                    Nothing -> ["Undeclared variable: " ++ varName]
                allVar :: TypeCatalog
                allVar = Map.unionWith const (globals d) s

data TypeCorrector = TypeCorrector
instance Transformation TypeCorrector where
    type From TypeCorrector = ()
    type To TypeCorrector = ()
    type Down TypeCorrector = TypeCatalog     -- global variable's type, in a declaration
    type Up TypeCorrector = ()
    type State TypeCorrector = TypeCatalog    -- local variable's types

instance Transformable TypeCorrector Entity where
    transform t _ d p@ProcDef{} = defaultTransform t (state tr) d p where
        tr = defaultTransform t def d p -- start with an empty local variable type catalog
    transform _ s _ p = Result p s def -- just definitions, not implementation, not need check/correct

instance Transformable TypeCorrector Variable where
    transform _ ls gs v@Variable{..} = Result v' (Map.insert varName typ' ls) def where
        v' = v {varType = typ'}
        typ' = case Map.lookup varName allVar of
            Just typ
                | varType == typ -> typ
                | otherwise      -> select varType typ
            Nothing -> varType
        allVar = Map.unionWith const gs ls

select :: Type -> Type -> Type
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
            select'' r1 r2 -- Selects the smaller size if one is unknown.
              | isSingleton r1 && isSingleton r2 = (r1 == r2, r2)
              | isFull r1 = (True, r2)
              | isFull r2 = (True, r1)
              | otherwise = (False, fullRange)
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
        result $ transform TypeCorrector def gs x where
            gs = state $ transform GlobalCollector def False procedure
            x 
                | showErr = result $ transform TypeCheck def (TypeCheckDown gs False) procedure
                | otherwise = procedure
