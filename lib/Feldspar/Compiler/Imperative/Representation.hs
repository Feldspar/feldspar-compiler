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

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.Representation where

import Data.Typeable
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Monoid

import Feldspar.Compiler.Error

import Feldspar.Range
import Feldspar.Core.Types (Length)

-- =================================================
-- == Data stuctures to store imperative programs ==
-- =================================================

data Module t = Module
    { entities                      :: [Entity t]
    }
    deriving (Typeable, Show, Eq)

data Entity t
    = StructDef
        { structName                :: String
        , structMembers             :: [StructMember t]
        }
    | TypeDef
        { actualType                :: Type
        , typeName                  :: String
        }
    | Proc
        { procName                  :: String
        , inParams                  :: [Variable t]
        , outParams                 :: [Variable t]
        , procBody                  :: Maybe (Block t)
        }
    | ValueDef
        { valVar                    :: Variable t
        , valValue                  :: Constant t
        }
    deriving (Typeable, Show, Eq)

data StructMember t = StructMember
    { structMemberName              :: String
    , structMemberType              :: Type
    }
    deriving (Typeable, Show, Eq)

data Block t = Block
    { locals                        :: [Declaration t]
    , blockBody                     :: Program t
    }
    deriving (Typeable, Show, Eq)

data Program t
    = Empty
        {
        }
    | Comment
        { isBlockComment            :: Bool
        , commentValue              :: String
        }
    | Assign
        { lhs                       :: Expression t
        , rhs                       :: Expression t
        }
    | ProcedureCall
        { procCallName              :: String
        , procCallParams            :: [ActualParameter t]
        }
    | Sequence
        { sequenceProgs             :: [Program t]
        }
    | Switch
        { scrutinee                 :: Expression t
        , alts                      :: [(Pattern t, Block t)]
        }
    | SeqLoop
        { sLoopCond                 :: Expression t
        , sLoopCondCalc             :: Block t
        , sLoopBlock                :: Block t
        }
    | ParLoop
        { pParallel                 :: Bool
        , pLoopCounter              :: Variable t
        , pLoopBound                :: Expression t
        , pLoopStep                 :: Expression t
        , pLoopBlock                :: Block t
        }
    | BlockProgram
        { blockProgram              :: Block t
        }
    deriving (Typeable, Show, Eq)

data Pattern t
   = PatDefault
   | Pat (Expression t)
     deriving (Typeable, Show, Eq)

data ActualParameter t
    = ValueParameter
        { valueParam                :: Expression t
        }
    | TypeParameter
        { typeParam                 :: Type
        }
    | FunParameter
        { funParamName              :: String
        }
    deriving (Typeable, Show, Eq)

data Declaration t = Declaration
    { declVar                       :: Variable t
    , initVal                       :: Maybe (Expression t)
    }
    deriving (Typeable, Show, Eq)

data Expression t
    = VarExpr
        { varExpr                   :: Variable t
        }
    | ArrayElem
        { array                     :: Expression t
        , arrayIndex                :: Expression t
        }
    | StructField
        { struct                    :: Expression t
        , fieldName                 :: String
        }
    | ConstExpr
        { constExpr                 :: Constant t
        }
    | FunctionCall
        { function                  :: Function
        , funCallParams             :: [Expression t]
        }
    | Cast
        { castType                  :: Type
        , castExpr                  :: Expression t
        }
    | AddrOf
        { addrExpr                  :: Expression t
        }
    | SizeOf
        { sizeOf                    :: Type
        }
    deriving (Typeable, Show, Eq)

data Function
    = Function
        { funName                   :: String
        , returnType                :: Type
        , funMode                   :: FunctionMode
        }
    deriving (Typeable, Show, Eq)

data Constant t
    = IntConst
        { intValue                  :: Integer
        , intType                   :: Type
        }
    | DoubleConst
        { doubleValue               :: Double
        }
    | FloatConst
        { floatValue                :: Float
        }
    | BoolConst
        { boolValue                 :: Bool
        }
    | ComplexConst
        { realPartComplexValue      :: Constant t
        , imagPartComplexValue      :: Constant t
        }
    | ArrayConst
        { arrayValues               :: [Constant t]
        }
    deriving (Typeable, Show, Eq)

data Variable t
    = Variable
        { varType                   :: Type
        , varName                   :: String
        }
    deriving (Typeable, Show, Eq)

instance Monoid (Program t)
  where
    mempty                              = Empty
    mappend Empty     p                 = p
    mappend p        Empty              = p
    mappend (Sequence pa) (Sequence pb) = Sequence (mappend pa pb)
    mappend pa pb                       = Sequence [mappend pa pb]

instance Monoid (Block t)
  where
    mempty                              = Block [] Empty
    mappend (Block da pa) (Block db pb) = Block (mappend da db) (mappend pa pb)


-- ======================
-- == Basic structures ==
-- ======================

data Size = S8 | S16 | S32 | S40 | S64
    deriving (Eq,Show)

data Signedness = Signed | Unsigned
    deriving (Eq,Show)

data Type =
      VoidType
    | BoolType
    | BitType
    | FloatType
    | DoubleType
    | NumType Signedness Size
    | ComplexType Type
    | UserType String
    | Alias Type String
    | ArrayType (Range Length) Type
    | NativeArray (Maybe Length) Type
    | StructType String [(String, Type)]
    | Pointer Type
    | IVarType Type
    deriving (Eq,Show)

data FunctionMode = Prefix | Infix
    deriving (Eq,Show)

----------------------
--   Type inference --
----------------------

class HasType a where
    type TypeOf a
    typeof :: a -> TypeOf a

instance HasType (Variable t) where
    type TypeOf (Variable t) = Type
    typeof Variable{..}      = varType

instance HasType (Constant t) where
    type TypeOf (Constant t) = Type
    typeof IntConst{..}      = intType
    typeof DoubleConst{}     = DoubleType
    typeof FloatConst{}      = FloatType
    typeof BoolConst{}       = BoolType
    typeof ArrayConst{..}    = NativeArray (Just (fromIntegral $ length arrayValues)) t
      where t = typeof $ head arrayValues
    typeof ComplexConst{..}  = ComplexType $ typeof realPartComplexValue

instance HasType (Expression t) where
    type TypeOf (Expression t) = Type
    typeof VarExpr{..}   = typeof varExpr
    typeof ArrayElem{..} = decrArrayDepth $ typeof array
      where
        decrArrayDepth :: Type -> Type
        decrArrayDepth (ArrayType _ t)   = t
        decrArrayDepth (NativeArray _ t) = t
        decrArrayDepth (Pointer t)       = decrArrayDepth t
        decrArrayDepth t                 = reprError InternalError $ "Non-array variable is indexed! " ++ show array ++ " :: " ++ show t
    typeof StructField{..} = getStructFieldType fieldName $ typeof struct
      where
        getStructFieldType :: String -> Type -> Type
        getStructFieldType f (StructType _ l) = fromMaybe (structFieldNotFound f) $ lookup f l
        getStructFieldType f (Alias t _) = getStructFieldType f t
        getStructFieldType f (Pointer t) = getStructFieldType f t
        getStructFieldType f t = reprError InternalError $
            "Trying to get a struct field from not a struct typed expression\n" ++ "Field: " ++ f ++ "\nType:  " ++ show t
        structFieldNotFound f = reprError InternalError $ "Not found struct field with this name: " ++ f
    typeof ConstExpr{..}    = typeof constExpr
    typeof FunctionCall{..} = returnType function
    typeof Cast{..}         = castType
    typeof AddrOf{..}       = Pointer $ typeof addrExpr
    typeof SizeOf{..}       = NumType Signed S32

instance HasType (ActualParameter t) where
    type TypeOf (ActualParameter t) = Type
    typeof ValueParameter{..}= typeof valueParam
    typeof TypeParameter{..} = typeParam
    typeof FunParameter{}    = VoidType


reprError :: forall a. ErrorClass -> String -> a
reprError = handleError "Feldspar.Compiler.Imperative.Representation"

-- | Free variables of an expression.
fv :: Expression t -> [Variable t]
fv = nub . fv'

fv' :: Expression t -> [Variable t]
fv' (VarExpr v)         = [v]
fv' (ArrayElem e i)     = fv' e ++ fv' i
fv' (StructField e _)   = fv' e
fv' (FunctionCall _ ps) = concatMap fv' ps
fv' (Cast _ e)          = fv' e
fv' (AddrOf e)          = fv' e
fv' _                   = []

