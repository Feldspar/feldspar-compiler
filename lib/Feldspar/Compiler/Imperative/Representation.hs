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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.Representation where

import Data.Typeable
import qualified Data.List as List (find)

import Feldspar.Compiler.Error

-- ===============================================================================================
-- == Class defining semantic information attached to different nodes in the imperative program ==
-- ===============================================================================================

class Annotation t (s :: * -> *) where
    type Label t s

instance Annotation () s where
    type Label () s = ()

-- =================================================
-- == Data stuctures to store imperative programs ==
-- =================================================

data Module t = Module
    { entities                      :: [Entity t]
    , moduleLabel                   :: Label t Module
    }
    deriving Typeable

deriving instance (ShowLabel t) => Show (Module t)
deriving instance (EqLabel t)   => Eq (Module t)

data Entity t
    = StructDef
        { structName                :: String
        , structMembers             :: [StructMember t]
        , structLabel               :: Label t Struct
        , definitionLabel           :: Label t Entity
        }
    | TypeDef
        { actualType                :: Type
        , typeName                  :: String
        , definitionLabel           :: Label t Entity
        }
    | ProcDef
        { procName                  :: String
        , inParams                  :: [Variable t]
        , outParams                 :: [Variable t]
        , procBody                  :: Block t
        , procDefLabel              :: Label t ProcDef
        , definitionLabel           :: Label t Entity
        }
    | ProcDecl
        { procName                  :: String
        , inParams                  :: [Variable t]
        , outParams                 :: [Variable t]
        , procDeclLabel             :: Label t ProcDecl
        , definitionLabel           :: Label t Entity
        }
    deriving Typeable

deriving instance (ShowLabel t) => Show (Entity t)
deriving instance (EqLabel t)   => Eq (Entity t) 

data StructMember t = StructMember
    { structMemberName              :: String
    , structMemberType              :: Type
    , structMemberLabel             :: Label t StructMember
    }
    deriving Typeable

deriving instance (ShowLabel t) => Show (StructMember t)
deriving instance (EqLabel t)   => Eq (StructMember t)

data Block t = Block
    { locals                        :: [Declaration t]
    , blockBody                     :: Program t
    , blockLabel                    :: Label t Block
    }
    deriving Typeable

deriving instance (ShowLabel t) => Show (Block t)
deriving instance (EqLabel t)   => Eq (Block t)

data Program t
    = Empty
        { emptyLabel                :: Label t Empty
        , programLabel              :: Label t Program
        }
    | Comment
        { isBlockComment            :: Bool
        , commentValue              :: String
        , commentLabel              :: Label t Comment
        , programLabel              :: Label t Program
        }
    | Assign
        { lhs                       :: Expression t
        , rhs                       :: Expression t
        , assignLabel               :: Label t Assign
        , programLabel              :: Label t Program
        }
    | ProcedureCall
        { procCallName              :: String
        , procCallParams            :: [ActualParameter t]
        , procCallLabel             :: Label t ProcedureCall
        , programLabel              :: Label t Program
        }
    | Sequence
        { sequenceProgs             :: [Program t]
        , sequenceLabel             :: Label t Sequence
        , programLabel              :: Label t Program
        }
    | Branch
        { branchCond                :: Expression t
        , thenBlock                 :: Block t
        , elseBlock                 :: Block t
        , branchLabel               :: Label t Branch
        , programLabel              :: Label t Program
        }
    | SeqLoop
        { sLoopCond                 :: Expression t
        , sLoopCondCalc             :: Block t
        , sLoopBlock                :: Block t
        , sLoopLabel                :: Label t SeqLoop
        , programLabel              :: Label t Program
        }
    | ParLoop
        { pLoopCounter              :: Variable t
        , pLoopBound                :: Expression t
        , pLoopStep                 :: Int
        , pLoopBlock                :: Block t
        , pLoopLabel                :: Label t ParLoop
        , programLabel              :: Label t Program
        }
    | BlockProgram
        { blockProgram              :: Block t
        , programLabel              :: Label t Program
        }
    deriving Typeable

deriving instance (ShowLabel t) => Show (Program t)
deriving instance (EqLabel t)   => Eq (Program t)

data ActualParameter t
    = In
        { inParam                   :: Expression t
        , actParamLabel             :: Label t ActualParameter
        }
    | Out
        { outParam                  :: Expression t
        , actParamLabel             :: Label t ActualParameter
        }
    | TypeParameter
        { typeParam                 :: Type
        , typeParamMode             :: TypeParameterMode
        , actParamLabel             :: Label t ActualParameter
        }
    | FunParameter
        { funParamName              :: String
        , addressNeeded             :: Bool
        , actParamLabel             :: Label t ActualParameter
        }
    deriving Typeable

deriving instance (ShowLabel t) => Show (ActualParameter t)
deriving instance (EqLabel t)   => Eq (ActualParameter t)

data Declaration t = Declaration
    { declVar                       :: Variable t
    , initVal                       :: Maybe (Expression t)
    , declLabel                     :: Label t Declaration
    }
    deriving Typeable

deriving instance (ShowLabel t) => Show (Declaration t)
deriving instance (EqLabel t)   => Eq (Declaration t)

data Expression t
    = VarExpr
        { var                       :: Variable t
        , exprLabel                 :: Label t Expression
        }
    | ArrayElem
        { array                     :: Expression t
        , arrayIndex                :: Expression t
        , arrayLabel                :: Label t ArrayElem
        , exprLabel                 :: Label t Expression
        }
    | StructField
        { struct                    :: Expression t
        , fieldName                 :: String
        , structFieldLabel          :: Label t StructField
        , exprLabel                 :: Label t Expression
        }
    | ConstExpr
        { constExpr                 :: Constant t
        , exprLabel                 :: Label t Expression
        }
    | FunctionCall
        { function                  :: Function
        , funCallParams             :: [Expression t]
        , funCallLabel              :: Label t FunctionCall
        , exprLabel                 :: Label t Expression
        }
    | Cast
        { castType                  :: Type
        , castExpr                  :: Expression t
        , castLabel                 :: Label t Cast
        , exprLabel                 :: Label t Expression
        }
    | SizeOf
        { sizeOf                    :: Either Type (Expression t)
        , sizeOfLabel               :: Label t SizeOf
        , exprLabel                 :: Label t Expression
        }
    deriving Typeable

deriving instance (ShowLabel t) => Show (Expression t)
deriving instance (EqLabel t)   => Eq (Expression t)

data Function = Function {
    funName                   :: String
  , returnType                :: Type
  , funMode                   :: FunctionMode
} deriving (Eq, Show)

data Constant t
    = IntConst
        { intValue                  :: Integer
        , intType                   :: Type
        , intConstLabel             :: Label t IntConst
        , constLabel                :: Label t Constant
        }
    | FloatConst
        { floatValue                :: Double
        , floatConstLabel           :: Label t FloatConst
        , constLabel                :: Label t Constant
        }
    | BoolConst
        { boolValue                 :: Bool
        , boolConstLabel            :: Label t BoolConst
        , constLabel                :: Label t Constant
        }
    | ComplexConst
        { realPartComplexValue       :: Constant t
        , imagPartComplexValue       :: Constant t
        , complexConstLabel          :: Label t ComplexConst
        , constLabel                 :: Label t Constant
        }
    deriving Typeable

deriving instance (ShowLabel t) => Show (Constant t)
deriving instance (EqLabel t)   => Eq (Constant t)

data Variable t = Variable
    { varName                        :: String
    , varType                        :: Type
    , varRole                        :: VariableRole
    , varLabel                       :: Label t Variable
    }
    deriving Typeable

deriving instance (ShowLabel t) => Show (Variable t)
deriving instance (EqLabel t)   => Eq (Variable t)

-- ======================
-- == Basic structures ==
-- ======================

data Length =
      LiteralLen Int
    | UndefinedLen
    deriving (Eq,Show)

data Size = S8 | S16 | S32 | S40 | S64
    deriving (Eq,Show)

data Signedness = Signed | Unsigned
    deriving (Eq,Show)

data Type =
      VoidType
    | BoolType
    | BitType
    | FloatType
    | NumType Signedness Size
    | ComplexType Type
    | UserType String
    | Alias Type String
    | ArrayType Length Type
    | StructType [(String, Type)]
    | IVarType Type
    deriving (Eq,Show)

data FunctionMode = Prefix | Infix
    deriving (Eq,Show)

data VariableRole = Value | Pointer
    deriving (Eq,Show)

data Place
    = Declaration_pl
    | MainParameter_pl
    | ValueNeed_pl
    | AddressNeed_pl
    | FunctionCallIn_pl
    deriving (Eq,Show)

data TypeParameterMode = Auto | Scalar
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

instance (ShowLabel t) => HasType (Constant t) where
    type TypeOf (Constant t) = Type
    typeof IntConst{..}      = intType
    typeof FloatConst{}      = FloatType
    typeof BoolConst{}       = BoolType
    typeof ComplexConst{..}  = ComplexType $ typeof realPartComplexValue

instance (ShowLabel t) => HasType (Expression t) where
    type TypeOf (Expression t) = Type
    typeof VarExpr{..}   = typeof var
    typeof ArrayElem{..} = decrArrayDepth $ typeof array
      where
        decrArrayDepth :: Type -> Type
        decrArrayDepth (ArrayType _ t) = t
        decrArrayDepth t               = reprError InternalError $ "Non-array variable is indexed! " ++ show array ++ " :: " ++ show t
    typeof StructField{..} = getStructFieldType fieldName $ typeof struct
      where
        getStructFieldType :: String -> Type -> Type
        getStructFieldType f (StructType l) = case List.find (\(a,_) -> a == f) l of
            Just (_,t) -> t
            Nothing -> structFieldNotFound f
        getStructFieldType f (Alias t _) = getStructFieldType f t
        getStructFieldType f t = reprError InternalError $
            "Trying to get a struct field from not a struct typed expression\n" ++ "Field: " ++ f ++ "\nType:  " ++ show t
        structFieldNotFound f = reprError InternalError $ "Not found struct field with this name: " ++ f
    typeof ConstExpr{..}    = typeof constExpr
    typeof FunctionCall{..} = returnType function
    typeof Cast{..}         = castType
    typeof SizeOf{..}       = NumType Signed S32

instance (ShowLabel t) => HasType (ActualParameter t) where
    type TypeOf (ActualParameter t) = Type
    typeof In{..}            = typeof inParam
    typeof Out{..}           = typeof outParam
    typeof TypeParameter{..} = typeParam
    typeof FunParameter{}    = VoidType


reprError :: forall a. ErrorClass -> String -> a
reprError = handleError "Feldspar.Compiler.Imperative.Representation"

-- =====================
-- == Technical types ==
-- =====================

data Struct t
data ProcDef t
data ProcDecl t
data Empty t
data Comment t
data Assign t
data ProcedureCall t
data Spawn t
data Run t
data Sequence t
data Branch t
data SeqLoop t
data ParLoop t
data FunctionCall t
data Cast t
data SizeOf t
data ArrayElem t
data StructField t
data LeftFunCall t
data IntConst t
data FloatConst t
data BoolConst t
data ArrayConst t
data ComplexConst t

-- ==========================
-- == Show and Eq instance ==
-- ==========================

class ( Show (Label t Module)
      , Show (Label t Entity)
      , Show (Label t Struct)
      , Show (Label t ProcDef)
      , Show (Label t ProcDecl)
      , Show (Label t StructMember)
      , Show (Label t Block)
      , Show (Label t Program)
      , Show (Label t Empty)
      , Show (Label t Comment)
      , Show (Label t Assign)
      , Show (Label t ProcedureCall)
      , Show (Label t Spawn)
      , Show (Label t Run)
      , Show (Label t Sequence)
      , Show (Label t Branch)
      , Show (Label t SeqLoop)
      , Show (Label t ParLoop)
      , Show (Label t ActualParameter)
      , Show (Label t Declaration)
      , Show (Label t Expression)
      , Show (Label t FunctionCall)
      , Show (Label t Cast)
      , Show (Label t SizeOf)
      , Show (Label t ArrayElem)
      , Show (Label t StructField)
      , Show (Label t Constant)
      , Show (Label t IntConst)
      , Show (Label t FloatConst)
      , Show (Label t BoolConst)
      , Show (Label t ArrayConst)
      , Show (Label t ComplexConst)
      , Show (Label t Variable)
      ) => ShowLabel t

instance ( Show (Label t Module)
         , Show (Label t Entity)
         , Show (Label t Struct)
         , Show (Label t ProcDef)
         , Show (Label t ProcDecl)
         , Show (Label t StructMember)
         , Show (Label t Block)
         , Show (Label t Program)
         , Show (Label t Empty)
         , Show (Label t Comment)
         , Show (Label t Assign)
         , Show (Label t ProcedureCall)
         , Show (Label t Spawn)
         , Show (Label t Run)
         , Show (Label t Sequence)
         , Show (Label t Branch)
         , Show (Label t SeqLoop)
         , Show (Label t ParLoop)
         , Show (Label t ActualParameter)
         , Show (Label t Declaration)
         , Show (Label t Expression)
         , Show (Label t FunctionCall)
         , Show (Label t Cast)
         , Show (Label t SizeOf)
         , Show (Label t ArrayElem)
         , Show (Label t StructField)
         , Show (Label t Constant)
         , Show (Label t IntConst)
         , Show (Label t FloatConst)
         , Show (Label t BoolConst)
         , Show (Label t ArrayConst)
         , Show (Label t ComplexConst)
         , Show (Label t Variable)
         ) => ShowLabel t

class ( Eq (Label t Module)
      , Eq (Label t Entity)
      , Eq (Label t Struct)
      , Eq (Label t ProcDef)
      , Eq (Label t ProcDecl)
      , Eq (Label t StructMember)
      , Eq (Label t Block)
      , Eq (Label t Program)
      , Eq (Label t Empty)
      , Eq (Label t Comment)
      , Eq (Label t Assign)
      , Eq (Label t ProcedureCall)
      , Eq (Label t Spawn)
      , Eq (Label t Run)
      , Eq (Label t Sequence)
      , Eq (Label t Branch)
      , Eq (Label t SeqLoop)
      , Eq (Label t ParLoop)
      , Eq (Label t ActualParameter)
      , Eq (Label t Declaration)
      , Eq (Label t Expression)
      , Eq (Label t FunctionCall)
      , Eq (Label t Cast)
      , Eq (Label t SizeOf)
      , Eq (Label t StructField)
      , Eq (Label t ArrayElem)
      , Eq (Label t Constant)
      , Eq (Label t IntConst)
      , Eq (Label t FloatConst)
      , Eq (Label t BoolConst)
      , Eq (Label t ArrayConst)
      , Eq (Label t ComplexConst)
      , Eq (Label t Variable)
      ) => EqLabel t

instance ( Eq (Label t Module)
         , Eq (Label t Entity)
         , Eq (Label t Struct)
         , Eq (Label t ProcDef)
         , Eq (Label t ProcDecl)
         , Eq (Label t StructMember)
         , Eq (Label t Block)
         , Eq (Label t Program)
         , Eq (Label t Empty)
         , Eq (Label t Comment)
         , Eq (Label t Assign)
         , Eq (Label t ProcedureCall)
         , Eq (Label t Spawn)
         , Eq (Label t Run)
         , Eq (Label t Sequence)
         , Eq (Label t Branch)
         , Eq (Label t SeqLoop)
         , Eq (Label t ParLoop)
         , Eq (Label t ActualParameter)
         , Eq (Label t Declaration)
         , Eq (Label t Expression)
         , Eq (Label t FunctionCall)
         , Eq (Label t Cast)
         , Eq (Label t SizeOf)
         , Eq (Label t StructField)
         , Eq (Label t ArrayElem)
         , Eq (Label t Constant)
         , Eq (Label t IntConst)
         , Eq (Label t FloatConst)
         , Eq (Label t BoolConst)
         , Eq (Label t ArrayConst)
         , Eq (Label t ComplexConst)
         , Eq (Label t Variable)
         ) => EqLabel t

-- * Set and get labels

class Labeled c where
    label       :: c t -> Label t c
    setLabel    :: c t -> Label t c -> c t

instance Labeled Module where
    label = moduleLabel
    setLabel c lab = c{ moduleLabel = lab }

instance Labeled Entity where
    label = definitionLabel
    setLabel c lab = c{ definitionLabel = lab }

instance Labeled Program where
    label = programLabel
    setLabel c lab = c{ programLabel = lab }

instance Labeled Declaration where
    label = declLabel
    setLabel c lab = c{ declLabel = lab }

instance Labeled Constant where
    label = constLabel
    setLabel c lab = c{ constLabel = lab }

instance Labeled StructMember where
    label = structMemberLabel
    setLabel c lab = c{ structMemberLabel = lab }

instance Labeled Variable where
    label = varLabel
    setLabel c lab = c{ varLabel = lab }

instance Labeled Expression where
    label = exprLabel
    setLabel c lab = c{ exprLabel = lab }

instance Labeled ActualParameter where
    label = actParamLabel
    setLabel c lab = c{ actParamLabel = lab }

instance Labeled Block where
    label = blockLabel
    setLabel c lab = c{ blockLabel = lab }
