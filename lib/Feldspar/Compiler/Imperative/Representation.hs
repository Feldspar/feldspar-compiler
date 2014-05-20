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
{-# LANGUAGE DeriveDataTypeable #-}

module Feldspar.Compiler.Imperative.Representation (
    Module(..)
  , Entity(..)
  , Declaration(..)
  , Block(..)
  , Program(..)
  , Expression(..)
  , ActualParameter(..)
  , Function(..)
  , Variable(..)
  , StructMember(..)
  , Pattern(..)
  , Type(..)
  , ScalarType(..)
  , Constant(..)
  , module Feldspar.Core.UntypedRepresentation
  , fv
  )
  where

import Data.Typeable
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Monoid

import Feldspar.Compiler.Error

import Feldspar.Range
import Feldspar.Core.Types (Length)
import Feldspar.Core.UntypedRepresentation ( Signedness(..), Size(..)
                                           , HasType(..))

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
        -- Left is regular return, right is fast return.
        , outParams                 :: Either [Variable t] (Variable t)
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
        { lhs                       :: Maybe (Expression t)
           -- ^ Nothing for effects only
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
    | Deref
        { ptrExpr                   :: Expression t
        }
    deriving (Typeable, Show, Eq)

data Function
    = Function
        { funName                   :: String
        , returnType                :: Type
        }
    deriving (Typeable, Show, Eq)

data Constant t
    = IntConst
        { intValue                  :: Integer
        , intType                   :: ScalarType
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

data ScalarType =
      BoolType
    | BitType
    | FloatType
    | DoubleType
    | NumType Signedness Size
    | ComplexType Type
    deriving (Eq,Show)

data Type =
      VoidType
    | MachineVector Length ScalarType
    | AliasType Type String
    | ArrayType (Range Length) Type
    | NativeArray (Maybe Length) Type
    | StructType String [(String, Type)]
    | Pointer Type
    | IVarType Type
    deriving (Show)

-- | Type equality is just structural equality, except for arrays
-- where size info is ignored and struct types where the tag is ignored.
instance Eq Type where
   VoidType              == VoidType              = True
   (MachineVector l1 t1) == (MachineVector l2 t2) = l1 == l2 && t1 == t2
   (AliasType t1 s1)     == (AliasType t2 s2)     = t1 == t2 && s1 == s2
   (ArrayType _ t1)      == (ArrayType _ t2)      = t1 == t2
   (NativeArray l1 t1)   == (NativeArray l2 t2)   = l1 == l2 && t1 == t2
   (StructType _ l1)     == (StructType _ l2)     = l1 == l2
   (Pointer t1)          == (Pointer t2)          = t1 == t2
   (IVarType t1)         == (IVarType t2)         = t1 == t2
   _                     == _                     = False


----------------------
--   Type inference --
----------------------

instance HasType (Variable t) where
    type TypeOf (Variable t) = Type
    typeof Variable{..}      = varType

instance HasType (Constant t) where
    type TypeOf (Constant t) = Type
    typeof IntConst{..}      = MachineVector 1 intType
    typeof DoubleConst{}     = MachineVector 1 DoubleType
    typeof FloatConst{}      = MachineVector 1 FloatType
    typeof BoolConst{}       = MachineVector 1 BoolType
    typeof ArrayConst{..}    = NativeArray (Just (fromIntegral $ length arrayValues)) t
      where t = typeof $ head arrayValues
    typeof ComplexConst{..}  = MachineVector 1 (ComplexType $ typeof realPartComplexValue)

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
        getStructFieldType f (AliasType t _)  = getStructFieldType f t
        getStructFieldType f (Pointer t)      = getStructFieldType f t
        getStructFieldType f t                = reprError InternalError $
            "Trying to get a struct field from not a struct typed expression\n" ++ "Field: " ++ f ++ "\nType:  " ++ show t
        structFieldNotFound f = reprError InternalError $ "Not found struct field with this name: " ++ f
    typeof ConstExpr{..}    = typeof constExpr
    typeof FunctionCall{..} = returnType function
    typeof Cast{..}         = castType
    typeof AddrOf{..}       = Pointer $ typeof addrExpr
    typeof SizeOf{..}       = MachineVector 1 $ NumType Signed S32
    typeof Deref{..}        = case typeof ptrExpr of
                                Pointer btype -> btype
                                wtype         -> reprError InternalError $ "Type of dereferenced expression " ++ show ptrExpr ++ " has type " ++ show wtype

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
fv' (Deref e)           = fv' e
fv' _                   = []

