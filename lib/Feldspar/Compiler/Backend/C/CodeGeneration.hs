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

{-# LANGUAGE FlexibleInstances #-}

module Feldspar.Compiler.Backend.C.CodeGeneration where

import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Error
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Backend.C.Library

import qualified Data.List as List (last,find)

-- =======================
-- == C code generation ==
-- =======================

codeGenerationError = handleError "CodeGeneration"

defaultMemberName = "member"

class ToC a where
    toC :: Options -> Place -> a -> String

getStructTypeName :: Options -> Place -> Type -> String
getStructTypeName options place t@(StructType types) =
    "_" ++ concat (map (\(_,t) -> (++"_") $ getStructTypeName options place t) types)
getStructTypeName options place t@(ArrayType len innerType) =
    "arr_T" ++ getStructTypeName options place innerType ++ "_S" ++ len2str len
    where
        len2str :: Length -> String
        len2str UndefinedLen = "UD"
        len2str (LiteralLen i) = show i
getStructTypeName options place t = replace (toC options place t) " " "" -- float complex -> floatcomplex

instance ToC Type where
    toC options MainParameter_pl VoidType = "void"
    toC options _ VoidType = "int"
    toC options place t@(StructType types) = "struct s" ++ getStructTypeName options place t
    toC options place (UserType u) = u
    toC options place (ArrayType _ _) = arrayTypeName
    toC options place (IVarType _) = ivarTypeName
    toC options place t = case (List.find (\(t',_,_) -> t == t') $ types $ platform options) of
        Just (_,s,_)  -> s
        Nothing       -> codeGenerationError InternalError $
                         "Unhandled type in platform " ++ (name $ platform options) ++ ": " ++ show t ++ " place: " ++ show place

instance ToC (Variable ()) where
    toC options place a@(Variable name typ role _) = show_variable options place role typ name

show_variable :: Options -> Place -> VariableRole -> Type -> String -> String
show_variable options place role typ name  = listprint id " " [variableType, show_name role place typ name] where
    variableType = show_type options role place typ restr
    restr
        | place == MainParameter_pl = isRestrict $ platform options
        | otherwise = NoRestrict

show_type :: Options -> VariableRole -> Place -> Type -> IsRestrict -> String
show_type options role MainParameter_pl t _
    | passByReference t || role == Pointer  = name ++ " *"
    | otherwise                             = name
  where
   name = toC options MainParameter_pl t
show_type options _ Declaration_pl t _ = toC options Declaration_pl t
show_type _ _ _ _ _ = ""

arrayTypeName = "struct array"
ivarTypeName = "struct ivar"

show_name :: VariableRole -> Place -> Type -> String  -> String
show_name Value place t n
    | place == AddressNeed_pl = "&" ++ n
    | place == FunctionCallIn_pl && passByReference t  = "&" ++ n
    | otherwise = n
show_name Pointer p (ArrayType _ _) n = n
show_name Pointer place t n
    | place == AddressNeed_pl = n
    | place == Declaration_pl = codeGenerationError InternalError $ "Output variable of the function declared!"
    | place == MainParameter_pl = n
    | otherwise = "(* " ++ n ++ ")"

passByReference :: Type -> Bool
passByReference (ArrayType _ _) = True
passByReference (StructType _) = True
passByReference _ = False

----------------------
-- Helper functions --
----------------------

ind :: (a-> String) -> a -> String
ind f x = unlines $ map (\a -> "    " ++ a) $ lines $ f x

listprint :: (a->String) -> String -> [a] -> String
listprint f s xs = listprint' s $ filter (\a -> a /= "") $ map f xs where
    listprint' _ [] = ""
    listprint' _ [x] = x
    listprint' s (x:xs) = x ++ s ++ listprint' s (xs)

decrArrayDepth :: Type -> Type
decrArrayDepth (ArrayType _ t) = t
decrArrayDepth t = codeGenerationError InternalError $ "Non-array variable of type " ++ show t ++ " is indexed!"

getStructFieldType :: String -> Type -> Type
getStructFieldType f (StructType l) = case List.find (\(a,_) -> a == f) l of
    Just (_,t) -> t
    Nothing -> structFieldNotFound f
getStructFieldType f t = codeGenerationError InternalError $
    "Trying to get a struct field from not a struct typed expression\n" ++ "Field: " ++ f ++ "\nType:  " ++ show t

structFieldNotFound f = codeGenerationError InternalError $ "Not found struct field with this name: " ++ f
