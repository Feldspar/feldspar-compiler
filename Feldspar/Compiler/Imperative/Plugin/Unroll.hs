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

{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Feldspar.Compiler.Imperative.Plugin.Unroll where

import Data.List (elem)

import Feldspar.Compiler.Backend.C.Options
import Feldspar.Transformation

-- ============================
-- == Unroll's Semantic info ==
-- ============================

data SemInfPrg = SemInfPrg
    {    position    :: Int
    ,    varNames    :: [String]
    ,    loopVar        :: String
    } deriving (Eq, Show)

data UnrollSemInf

instance Annotation UnrollSemInf Module where
    type Label UnrollSemInf Module = ()

instance Annotation UnrollSemInf Entity where
    type Label UnrollSemInf Entity = ()

instance Annotation UnrollSemInf Struct where
    type Label UnrollSemInf Struct = ()

instance Annotation UnrollSemInf StructMember where
    type Label UnrollSemInf StructMember = ()

instance Annotation UnrollSemInf ProcDef where
    type Label UnrollSemInf ProcDef = ()

instance Annotation UnrollSemInf ProcDecl where
    type Label UnrollSemInf ProcDecl = ()

instance Annotation UnrollSemInf Block where
    type Label UnrollSemInf Block = ()

instance Annotation UnrollSemInf Program where
    type Label UnrollSemInf Program = Maybe SemInfPrg

instance Annotation UnrollSemInf Empty where
    type Label UnrollSemInf Empty = ()

instance Annotation UnrollSemInf Comment where
    type Label UnrollSemInf Comment = ()

instance Annotation UnrollSemInf Assign where
    type Label UnrollSemInf Assign = ()

instance Annotation UnrollSemInf ProcedureCall where
    type Label UnrollSemInf ProcedureCall = ()

instance Annotation UnrollSemInf Spawn where
    type Label UnrollSemInf Spawn = ()

instance Annotation UnrollSemInf Run where
    type Label UnrollSemInf Run = ()

instance Annotation UnrollSemInf Sequence where
    type Label UnrollSemInf Sequence = ()

instance Annotation UnrollSemInf Branch where
    type Label UnrollSemInf Branch = ()

instance Annotation UnrollSemInf SeqLoop where
    type Label UnrollSemInf SeqLoop = ()

instance Annotation UnrollSemInf ParLoop where
    type Label UnrollSemInf ParLoop = ()

instance Annotation UnrollSemInf ActualParameter where
    type Label UnrollSemInf ActualParameter = ()

instance Annotation UnrollSemInf Declaration where
    type Label UnrollSemInf Declaration = ()

instance Annotation UnrollSemInf Expression where
    type Label UnrollSemInf Expression = ()

instance Annotation UnrollSemInf FunctionCall where
    type Label UnrollSemInf FunctionCall = ()

instance Annotation UnrollSemInf Cast where
    type Label UnrollSemInf Cast = ()

instance Annotation UnrollSemInf SizeOf where
    type Label UnrollSemInf SizeOf = ()

instance Annotation UnrollSemInf ArrayElem where
    type Label UnrollSemInf ArrayElem = ()

instance Annotation UnrollSemInf StructField where
    type Label UnrollSemInf StructField = ()

instance Annotation UnrollSemInf Constant where
    type Label UnrollSemInf Constant = ()

instance Annotation UnrollSemInf IntConst where
    type Label UnrollSemInf IntConst = ()

instance Annotation UnrollSemInf FloatConst where
    type Label UnrollSemInf FloatConst = ()

instance Annotation UnrollSemInf BoolConst where
    type Label UnrollSemInf BoolConst = ()

instance Annotation UnrollSemInf ArrayConst where
    type Label UnrollSemInf ArrayConst = ()

instance Annotation UnrollSemInf ComplexConst where
    type Label UnrollSemInf ComplexConst = ()

instance Annotation UnrollSemInf Variable where
    type Label UnrollSemInf Variable = ()

-- ==
-- == Plugin
-- ==

instance Default Bool where
    def = False

instance Combine Bool where
    combine = (||)

instance Default (Maybe SemInfPrg) where def = Nothing    


instance Plugin UnrollPlugin where
    type ExternalInfo UnrollPlugin = UnrollStrategy
    executePlugin UnrollPlugin ei p = case ei of
        NoUnroll -> p
        Unroll unrollCount -> result $ transform Unroll_2 () Nothing $ result $ transform Unroll_1 () unrollCount p
    
data UnrollPlugin = UnrollPlugin
instance Transformation UnrollPlugin where
    type From UnrollPlugin      = ()
    type To UnrollPlugin        = ()
    type Down UnrollPlugin      = ()
    type Up UnrollPlugin        = ()
    type State UnrollPlugin     = ()

data Unroll_1 = Unroll_1
instance Transformation Unroll_1 where
    type From Unroll_1      = ()
    type To Unroll_1        = UnrollSemInf
    type Down Unroll_1      = Int
    type Up Unroll_1        = Bool
    type State Unroll_1     = ()

instance Transformable Unroll_1 Program where
    transform t s d p@(ParLoop _ _ _ _ _ _)
        | up tr == False && unrollPossible = tr'
        | otherwise = tr
        where
        tr = defaultTransform t s d p
        tr' = tr 
            { result = (result tr)
                { pLoopStep = d
                , pLoopBlock = loopCore
                    { locals = unrollDecls
                    , blockBody = Sequence prgs () Nothing
                    }
                }
            , up = True
            }
        prgs = map (\(i,p) -> p{ programLabel = (Just $ SemInfPrg i varNames loopCounter) }) $ zip [0,1..] replPrg
        replPrg = replicate d $ blockBody loopCore
        unrollDecls = concat $ map (\(i,ds) -> renameDecls ds i) $ zip [0,1..] replDecls
        renameDecls ds i = map (\d -> renameDeclaration d ((getVarNameDecl d) ++ "_u" ++ (show i))) ds
        replDecls = replicate d $ locals loopCore
        loopCore = pLoopBlock $ result tr 
        loopBound = pLoopBound $ result tr
        loopCounter = varName $ pLoopCounter $ result tr
        varNames = map (\d -> getVarNameDecl d) $ locals loopCore
        unrollPossible = case loopBound of
            (ConstExpr (IntConst i _ _ _) _) -> mod i (toInteger d) == 0
            _                              -> False
    transform t s d p = defaultTransform t s d p


data Unroll_2 = Unroll_2    
instance Transformation Unroll_2     where
    type From Unroll_2      = UnrollSemInf
    type To Unroll_2        = ()
    type Down Unroll_2      = Maybe SemInfPrg
    type Up Unroll_2        = ()
    type State Unroll_2     = ()

instance Transformable Unroll_2 Program where
    transform t s d p = defaultTransform t s d' p where
        d' = case programLabel p of
            Nothing -> d
            x       -> x 

instance Transformable Unroll_2 Expression where
    transform t s d l = case d of
        Nothing -> tr
        Just x ->  case l of
            VarExpr n _
                | varName n == loopVar x -> tr 
                    { result = FunctionCall 
                        { function = Function
                            { funName = "+"
                            , returnType = NumType Signed S32
                            , funMode = Infix
                            }
                        , funCallParams = 
                            [ result tr
                            , ConstExpr (IntConst (toInteger $ position x) (NumType Signed S32) () ()) ()
                            ]
                        , funCallLabel = ()
                        , exprLabel = ()
                        }
                    }
                | otherwise ->  tr
            _ ->  tr
        where
            tr = defaultTransform t s d l


instance Transformable Unroll_2 Variable where
    transform t s d v = case d of
        Just x
            | (varName v) `elem` (varNames x) -> tr
                { result = (result tr)
                    { varName = (varName v) ++ "_u" ++ (show $ position x)
                    , varLabel = ()
                    }
                }
            | otherwise -> tr
        Nothing -> tr
        where
            tr = defaultTransform t s d v


-- helper functions : 
isJust (Just x) = True
isJust _ = False
getVarNameDecl d = varName $ declVar d
renameDeclaration d n = d { declVar = renameVariable (declVar d) n }
renameVariable v n = v { varName = n    }

