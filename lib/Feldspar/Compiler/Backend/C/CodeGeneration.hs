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

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Feldspar.Compiler.Backend.C.CodeGeneration where

import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Error (handleError, ErrorClass(..))
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Backend.C.Library

import Feldspar.Range (isSingleton, upperBound, Range(..))
import Feldspar.Core.Types (Length)

import Data.List (intercalate)

import Text.PrettyPrint

-- =======================
-- == C code generation ==
-- =======================

codeGenerationError :: ErrorClass -> String -> a
codeGenerationError = handleError "CodeGeneration"

toC :: Options -> Type -> Doc
toC _ VoidType                = text "void"
toC _ ArrayType{}             = arrayTypeName
toC _ IVarType{}              = ivarTypeName
toC _ (UserType u)            = text u
toC _ t@(StructType n _)      = text "struct" <+> text n
toC o (NativeArray _ t)       = toC o t
toC o t | [s] <- [s | (t',s,_) <- types $ platform o, t'==t] = text s
toC o t = codeGenerationError InternalError
        $ unwords ["Unhandled type in platform ", name (platform o),  ": ", show t]

arrayTypeName :: Doc
arrayTypeName = text "struct array"

ivarTypeName :: Doc
ivarTypeName = text "struct ivar"

passByReference :: Type -> Bool
passByReference ArrayType{}  = True
passByReference StructType{} = True
passByReference _            = False

