--
-- Copyright (c) 2020, ERICSSON AB
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

-- | Generation of type specific array management functions.
module Feldspar.Compiler.Imperative.ArrayOps (arrayOps) where

import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Imperative.Frontend
        (litI32, deepCopy, arrayLength, fun, call, for, toBlock, mkIf, isShallow, variant, encodeType, arrayFun)
import Feldspar.Range (Range(..), fullRange)
import Feldspar.Core.Types(Length)
import Feldspar.Compiler.Backend.C.Options(Options)

import Data.List (nub, isPrefixOf, concatMap)

-- | Main interface for adding needed array operations to a module.
arrayOps :: Options -> Module () -> Module ()
arrayOps opts (Module ents) = Module $ concatMap mkArrayOps dts ++ ents
  where dts = filter (not . either isShallow isShallow) (findCopyTypes ents)
        mkArrayOps (Left  t) = [mkInitArray opts t, mkFreeArray opts t]
        mkArrayOps (Right t) = [mkCopyArrayPos opts t, mkCopyArray opts t, mkInitCopyArray opts t]

-- | Copying an array to a given position in the destination
mkCopyArrayPos :: Options -> Type -> Entity ()
mkCopyArrayPos opts t = Proc name False [dstVar, srcVar, posVar] (Right dstVar) (Just body)
  where name = variant "copyArrayPos" t
        body = Block decls prog
        decls = []
        ixVar = Variable intT "i"
        srcVar = Variable (ArrayType fullRange t) "src"
        dstVar = Variable (ArrayType fullRange t) "dst"
        posVar = Variable intT "pos"
        prog = Sequence
                 [ for Parallel ixVar zero (arrayLength $ VarExpr srcVar) one loopBody
                 , call "return" [ValueParameter $ VarExpr dstVar]
                 ]
        loopBody = Block lbDecls (Sequence lbProg)
        lbDecls = []
        lbProg = unSeq $ deepCopy (ArrayElem (VarExpr dstVar) [fun intT "+" [VarExpr posVar, VarExpr ixVar]])
                                  [ArrayElem (VarExpr srcVar) [VarExpr ixVar]]
        unSeq (Sequence ps) = ps
        unSeq p             = [p]

-- | Copying an array to the beginning of another array
mkCopyArray :: Options -> Type -> Entity ()
mkCopyArray opts t = Proc name False [dstVar, srcVar] (Right dstVar) (Just body)
  where name = variant "copyArray" t
        srcVar = Variable (ArrayType fullRange t) "src"
        dstVar = Variable (ArrayType fullRange t) "dst"
        body = Block decls prog
        decls = []
        prog = Sequence
               [ Assign (VarExpr dstVar)
                        (fun (ArrayType fullRange t) (variant "copyArrayPos" t) [VarExpr dstVar, VarExpr srcVar, zero])
               , call "return" [ValueParameter $ VarExpr dstVar]
               ]

-- | Initializing and copying in a single operation
mkInitCopyArray :: Options -> Type -> Entity ()
mkInitCopyArray opts t = Proc name False [dstVar, srcVar] (Right dstVar) (Just body)
  where name = variant "initCopyArray" t
        srcVar = Variable (ArrayType fullRange t) "src"
        dstVar = Variable (ArrayType fullRange t) "dst"
        body = Block decls prog
        decls = []
        prog = Sequence
               [ Assign (VarExpr dstVar) (arrayFun "initArray" [VarExpr dstVar, arrayLength $ VarExpr srcVar])
               , Assign (VarExpr dstVar)
                        (arrayFun "copyArrayPos" [VarExpr dstVar, VarExpr srcVar, zero])
               , call "return" [ValueParameter $ VarExpr dstVar]
               ]

-- | Initialize an array to a given length
mkInitArray :: Options -> Type -> Entity ()
mkInitArray opts t = Proc name False [dstVar, newLen] (Right dstVar) (Just body)
  where name = variant "initArray" t
        dstVar = Variable (ArrayType fullRange t) "dst"
        oldLen = Variable lengthT "oldLen"
        newLen = Variable lengthT "newLen"
        body = Block decls prog
        decls = [Declaration oldLen $ Nothing]
        prog = Sequence [ Assign (VarExpr dstVar) (fun arrT "allocArray" [VarExpr dstVar])
                        , Assign (VarExpr oldLen) (arrayLength $ VarExpr dstVar)
                        , mkIf (fun boolT "/=" [VarExpr oldLen, VarExpr newLen]) (toBlock setLength) Nothing
                        , call "return" [ValueParameter $ VarExpr dstVar]
                        ]
        setLength = Sequence [ mkIf (fun boolT "<" [VarExpr oldLen, VarExpr newLen]) (toBlock grow) (Just $ toBlock shrink)
                             ]
        grow = Sequence [ Assign (VarExpr dstVar) (fun arrT "resizeArray" [VarExpr dstVar, SizeOf t, VarExpr newLen])
                        , for Parallel ixVar (VarExpr oldLen) (VarExpr newLen) one (Block initBodyDecls initBody)
                        ]
        initBody = Sequence [Assign e (VarExpr $ nullVar t i) | ((e,t),i) <- zip arrs [0..]]
        initBodyDecls = [Declaration (nullVar t i) Nothing |  ((e,t),i) <- zip arrs [0..]]
        shrink = Sequence [ for Parallel ixVar (VarExpr newLen) (VarExpr oldLen) one (toBlock freeBody)
                          , Assign (VarExpr dstVar) (fun arrT "resizeArray" [VarExpr dstVar, SizeOf t, VarExpr newLen])
                          ]
        freeBody = Sequence [call (variant "freeArray" t) [ValueParameter e] | (e,t) <- arrs]

        arrs = arrays (ArrayElem (VarExpr dstVar) [VarExpr ixVar]) t

        ixVar = Variable intT "i"
        nullVar t n = Variable (ArrayType fullRange t) ("null_arr_" ++ show n)
        arrT = ArrayType fullRange t

-- | Free an array
mkFreeArray :: Options -> Type -> Entity ()
mkFreeArray opts t = Proc name False [srcVar] (Left []) (Just body)
  where name = variant "freeArray" t
        srcVar = Variable (ArrayType fullRange t) "src"
        body = Block [] $ Sequence stms
        stms = [ for Parallel ixVar zero (arrayLength $ VarExpr srcVar) one loopBody
               , call "freeArray" [ValueParameter $ VarExpr srcVar]
               ]
        ixVar = Variable intT "i"
        loopBody = toBlock $ Sequence [call (variant "freeArray" t) [ValueParameter e] | (e,t) <- arrs]
        arrs = arrays (ArrayElem (VarExpr srcVar) [VarExpr ixVar]) t

-- | Type names
lengthT, intT, boolT :: Type
lengthT = 1 :# NumType Unsigned S32
intT    = 1 :# NumType Signed S32
boolT   = 1 :# BoolType

-- | Extract all arrays nested in a type
arrays :: Expression () -> Type -> [(Expression (), Type)]
arrays e (ArrayType _ t) = [(e,t)]
arrays e (NativeArray _ t) = [(e,t)]
arrays e (StructType _ fs) = concat [arrays (StructField e f) t | (f,t) <- fs]
arrays _ _                 = []

-- | Find types that need array management functions
findCopyTypes :: [Entity ()] -> [Either Type Type]
findCopyTypes es = nub $ concatMap ctEnt es
  where ctEnt Proc{procBody = Just b} = ctBlock b
        ctEnt _                  = []

        ctBlock b = ctProg $ blockBody b

        ctProg (Assign l e@(FunctionCall (Function "copy" _) _)) = let ts = eTypes e $ typeof l in map Right ts ++ map Left ts
        ctProg (Assign l e@(FunctionCall (Function name _) _)) | "initArray" `isPrefixOf` name = map Left $ eTypes e $ typeof l
        ctProg (Assign _ _) = []
        ctProg (Sequence ps) = concatMap ctProg ps
        ctProg (Switch _ alts) = concatMap (ctBlock . snd) alts
        ctProg (SeqLoop _ calc body) = ctBlock calc ++ ctBlock body
        ctProg (ParLoop _ _ _ _ _ body) = ctBlock body
        ctProg _ = []

        eTypes _ (ArrayType _ t) = [t]
        eTypes _ (NativeArray _ t) = [t]
        eTypes e t = error $ "ArrayOps.eType: surprising array type " ++ show t ++ " with rhs\n  " ++ show e

-- | Utilities
zero = litI32 0
one  = litI32 1
