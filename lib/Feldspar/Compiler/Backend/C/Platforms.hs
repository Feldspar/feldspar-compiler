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

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Feldspar.Compiler.Backend.C.Platforms
    ( availablePlatforms
    , c99
    , c99OpenMp
    , tic64x
    , c99Rules
    , tic64xRules
    , traceRules
    , nativeArrayRules
    ) where

import Data.Maybe (fromMaybe)

import Feldspar.Range
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Imperative.Frontend

availablePlatforms :: [Platform]
availablePlatforms = [ c99, c99OpenMp, tic64x ]

c99 :: Platform
c99 = Platform {
    name = "c99",
    types =
        [ (NumType Signed S8,     "int8_t")
        , (NumType Signed S16,    "int16_t")
        , (NumType Signed S32,    "int32_t")
        , (NumType Signed S64,    "int64_t")
        , (NumType Unsigned S8,   "uint8_t")
        , (NumType Unsigned S16,  "uint16_t")
        , (NumType Unsigned S32,  "uint32_t")
        , (NumType Unsigned S64,  "uint64_t")
        , (BoolType,              "uint32_t")
        , (FloatType,             "float")
        , (DoubleType,            "double")
        , (ComplexType FloatType, "float complex")
        , (ComplexType DoubleType,"double complex")
        ] ,
    values =
        [ (ComplexType FloatType, \cx -> "(" ++ showRe cx ++ "+" ++ showIm cx ++ "i)")
        , (ComplexType DoubleType, \cx -> "(" ++ showRe cx ++ "+" ++ showIm cx ++ "i)")
        , (BoolType, \b -> if boolValue b then "true" else "false")
        ] ,
    includes =
        [ "feldspar_c99.h"
        , "feldspar_array.h"
        , "feldspar_future.h"
        , "ivar.h"
        , "taskpool.h"
        , "<stdint.h>"
        , "<string.h>"
        , "<math.h>"
        , "<stdbool.h>"
        , "<complex.h>"],
    platformRules = c99Rules ++ traceRules ++ arrayRules,
    varFloating = True,
    isRestrict = NoRestrict
}

c99OpenMp :: Platform
c99OpenMp = c99 { name = "c99OpenMp"
                , varFloating = False
                }

tic64x :: Platform
tic64x = Platform {
    name = "tic64x",
    types =
        [ (NumType Signed S8,     "char")
        , (NumType Signed S16,    "short")
        , (NumType Signed S32,    "int")
        , (NumType Signed S40,    "long")
        , (NumType Signed S64,    "long long")
        , (NumType Unsigned S8,   "unsigned char")
        , (NumType Unsigned S16,  "unsigned short")
        , (NumType Unsigned S32,  "unsigned")
        , (NumType Unsigned S40,  "unsigned long")
        , (NumType Unsigned S64,  "unsigned long long")
        , (BoolType,              "int")
        , (FloatType,             "float")
        , (DoubleType,            "double")
        , (ComplexType FloatType, "complexOf_float")
        , (ComplexType DoubleType,"complexOf_double")
        ] ,
    values = 
        [ (ComplexType FloatType, \cx -> "complex_fun_float(" ++ showRe cx ++ "," ++ showIm cx ++ ")")
        , (ComplexType DoubleType, \cx -> "complex_fun_double(" ++ showRe cx ++ "," ++ showIm cx ++ ")")
        , (BoolType, \b -> if boolValue b then "1" else "0")
        ] ,
    includes = ["feldspar_tic64x.h", "feldspar_array.h", "<c6x.h>", "<string.h>", "<math.h>"],
    platformRules = tic64xRules ++ c99Rules ++ traceRules,
    varFloating = True,
    isRestrict = Restrict
}

showRe, showIm :: Constant t -> String
showRe = showConstant . realPartComplexValue
showIm = showConstant . imagPartComplexValue

showConstant :: Constant t -> String
showConstant (DoubleConst c) = show c ++ "f"
showConstant (FloatConst c)  = show c ++ "f"
showConstant c               = show c

arrayRules :: [Rule]
arrayRules = [rule copy]
  where
    copy (ProcedureCall "copy" [ValueParameter arg1, ValueParameter arg2])
        | arg1 == arg2
        = [replaceWith Empty]

        | ConstExpr ArrayConst{..} <- arg2
        = [replaceWith $ Sequence $ initArray (Just arg1) (litI32 $ toInteger $ length arrayValues):zipWith (\i c -> Assign (ArrayElem arg1 (litI32 i)) (ConstExpr c)) [0..] arrayValues]

        | NativeArray{} <- typeof arg2
        , l@(ConstExpr (IntConst n _)) <- arrayLength arg2
        = [replaceWith $ Sequence $ initArray (Just arg1) l:map (\i -> Assign (ArrayElem arg1 (litI32 i)) (ArrayElem arg2 (litI32 i))) [0..(n-1)]]

        | not (isArray (typeof arg1))
        = [replaceWith $ Assign arg1 arg2]

    copy (ProcedureCall "copy" (dst@(ValueParameter arg1):ins'@(ValueParameter in1:ins)))
        | isArray (typeof arg1)
        = [replaceWith $ Sequence ([
               initArray (Just arg1) (foldr ePlus (litI32 0) aLens)
             , if arg1 == in1 then Empty else call "copyArray" [dst, ValueParameter in1]
             ] ++ flattenCopy dst ins argnLens arg1len)]
           where
             aLens@(arg1len:argnLens) = map (\(ValueParameter src) -> arrayLength src) ins'

    copy (ProcedureCall "copy" _) = error "Multiple scalar arguments to copy"

    copy _ = []

nativeArrayRules :: [Rule]
nativeArrayRules = [rule toNativeExpr, rule toNativeProg, rule toNativeVariable]
  where
    toNativeExpr :: Expression () -> [Action (Expression ())]
    toNativeExpr _ = []

    toNativeProg (Assign _ (FunctionCall (Function "initArray" _ _) [arr,_,_]))
      | native (typeof arr) = [replaceWith $ call "assert" [ValueParameter arr]]
    toNativeProg (ProcedureCall "freeArray" [ValueParameter arr])
      | native (typeof arr) = [replaceWith Empty]
    toNativeProg _ = []


    toNativeVariable :: Variable () -> [Action (Variable ())]
    toNativeVariable v@Variable{..} | isArray varType
      = [replaceWith $ v { varType = nativeArray varType }]
    toNativeVariable _ = []

    nativeArray (Pointer (ArrayType sz t)) = NativeArray (fromSingleton sz) (nativeArray t)    
    nativeArray (ArrayType sz t) = NativeArray (fromSingleton sz) (nativeArray t)
    nativeArray (Pointer t)      = Pointer (nativeArray t)
    nativeArray t = t

    native (NativeArray {}) = True
    native (Pointer t)      = native t
    native _                = False

    fromSingleton r = if isSingleton r
                        then Just $ upperBound r
                        else Nothing

flattenCopy :: ActualParameter () -> [ActualParameter ()] -> [Expression ()] -> Expression () -> [Program ()]
flattenCopy _ [] [] _ = []
flattenCopy dst (ValueParameter t:ts) (l:ls) cLen = call "copyArrayPos" [dst, ValueParameter cLen, ValueParameter t]
                                   : flattenCopy dst ts ls (ePlus cLen l)

ePlus :: Expression () -> Expression () -> Expression ()
ePlus (ConstExpr (IntConst 0 _)) e = e
ePlus e (ConstExpr (IntConst 0 _)) = e
ePlus e1 e2 = binop (NumType Signed S32) "+" e1 e2

c99Rules :: [Rule]
c99Rules = [rule go]
  where
    go :: Expression () -> [Action (Expression ())]
    go (FunctionCall (Function "(!)" _ _) [arg1,arg2])    = [replaceWith $ ArrayElem arg1 arg2]
    go (FunctionCall (Function "getFst" _ _) [arg]) = [replaceWith $ StructField arg first]
    go (FunctionCall (Function "getSnd" _ _) [arg]) = [replaceWith $ StructField arg second]
    go (FunctionCall (Function "(==)" t _) [arg1, arg2])  = [replaceWith $ binop t "==" arg1 arg2]
    go (FunctionCall (Function "(/=)" t _) [arg1, arg2])  = [replaceWith $ binop t "!=" arg1 arg2]
    go (FunctionCall (Function "(<)" t _) [arg1, arg2])   = [replaceWith $ binop t "<" arg1 arg2]
    go (FunctionCall (Function "(>)" t _) [arg1, arg2])   = [replaceWith $ binop t ">" arg1 arg2]
    go (FunctionCall (Function "(<=)" t _) [arg1, arg2])  = [replaceWith $ binop t "<=" arg1 arg2]
    go (FunctionCall (Function "(>=)" t _) [arg1, arg2])  = [replaceWith $ binop t ">=" arg1 arg2]
    go (FunctionCall (Function "not" t _) [arg])  = [replaceWith $ fun t "!" [arg]]
    go (FunctionCall (Function "(&&)" t _) [arg1, arg2])  = [replaceWith $ binop t "&&" arg1 arg2]
    go (FunctionCall (Function "(||)" t _) [arg1, arg2])  = [replaceWith $ binop t "||" arg1 arg2]
    go (FunctionCall (Function "quot" t _) [arg1, arg2])  = [replaceWith $ binop t "/" arg1 arg2]
    go (FunctionCall (Function "rem" t _) [arg1, arg2])   = [replaceWith $ binop t "%" arg1 arg2]
    go (FunctionCall (Function "(^)" t _) [arg1, arg2])   = [replaceWith $ fun t (extend c99 "pow" t) [arg1, arg2]]
    go (FunctionCall (Function "abs" t _) [arg])  = [replaceWith $ fun t (extend c99 "abs" t) [arg]]
    go (FunctionCall (Function "signum" t _) [arg])   = [replaceWith $ fun t (extend c99 "signum" t) [arg]]
    go (FunctionCall (Function "(+)" t _) [arg1, arg2])   = [replaceWith $ binop t "+" arg1 arg2]
    go (FunctionCall (Function "(-)" t _) [ConstExpr (IntConst 0 _), arg2]) = [replaceWith $ fun t "-" [arg2]]
    go (FunctionCall (Function "(-)" t _) [ConstExpr (FloatConst 0), arg2]) = [replaceWith $ fun t "-" [arg2]]
    go (FunctionCall (Function "(-)" t _) [arg1, arg2])   = [replaceWith $ binop t "-" arg1 arg2]
    go (FunctionCall (Function "(*)" t _) [ConstExpr (IntConst (log2 -> Just n) _), arg2])    = [replaceWith $ binop t "<<" arg2 (litI32 n)]
    go (FunctionCall (Function "(*)" t _) [arg1, ConstExpr (IntConst (log2 -> Just n) _)])    = [replaceWith $ binop t "<<" arg1 (litI32 n)]
    go (FunctionCall (Function "(*)" t _) [arg1, arg2])   = [replaceWith $ binop t "*" arg1 arg2]
    go (FunctionCall (Function "(/)" t _) [arg1, arg2])   = [replaceWith $ binop t "/" arg1 arg2]
    go (FunctionCall (Function "div" t _) [arg1, arg2]) = [replaceWith $ StructField (fun div_t (div_f t) [arg1, arg2]) "quot"]
      where div_t = Alias (StructType "div_t" [("quot", t), ("rem", t)]) "div_t"
            div_f (NumType Signed S8)  = "div"
            div_f (NumType Signed S16) = "div"
            div_f (NumType Signed S32) = "div"
            div_f (NumType Signed S40) = "ldiv"
            div_f (NumType Signed S64) = "lldiv"
            div_f typ = error $ "div not defined for " ++ show typ
    go (FunctionCall (Function "exp" t@(ComplexType _) _) [arg])  = [replaceWith $ fun t "cexpf" [arg]]
    go (FunctionCall (Function "exp" t _) [arg])  = [replaceWith $ fun t "expf" [arg]]
    go (FunctionCall (Function "sqrt" t@(ComplexType _) _) [arg]) = [replaceWith $ fun t "csqrtf" [arg]]
    go (FunctionCall (Function "sqrt" t _) [arg]) = [replaceWith $ fun t "sqrtf" [arg]]
    go (FunctionCall (Function "log" t@(ComplexType _) _) [arg])  = [replaceWith $ fun t "clogf" [arg]]
    go (FunctionCall (Function "log" t _) [arg])  = [replaceWith $ fun t "logf" [arg]]
    go (FunctionCall (Function "(**)" t@(ComplexType _) _) [arg1, arg2])  = [replaceWith $ fun t "cpowf" [arg1,arg2]]
    go (FunctionCall (Function "(**)" t _) [arg1, arg2])  = [replaceWith $ fun t "powf" [arg1,arg2]]
    go (FunctionCall (Function "logBase" t _) [arg1, arg2])   = [replaceWith $ fun t (extend c99 "logBase" t) [arg1,arg2]]
    go (FunctionCall (Function "sin" t@(ComplexType _) _) [arg])  = [replaceWith $ fun t "csinf" [arg]]
    go (FunctionCall (Function "sin" t _) [arg])  = [replaceWith $ fun t "sinf" [arg]]
    go (FunctionCall (Function "tan" t@(ComplexType _) _) [arg])  = [replaceWith $ fun t "ctanf" [arg]]
    go (FunctionCall (Function "tan" t _) [arg])  = [replaceWith $ fun t "tanf" [arg]]
    go (FunctionCall (Function "cos" t@(ComplexType _)  _) [arg])  = [replaceWith $ fun t "ccosf" [arg]]
    go (FunctionCall (Function "cos" t _) [arg])  = [replaceWith $ fun t "cosf" [arg]]
    go (FunctionCall (Function "asin" t@(ComplexType _) _) [arg]) = [replaceWith $ fun t "casinf" [arg]]
    go (FunctionCall (Function "asin" t _) [arg]) = [replaceWith $ fun t "asinf" [arg]]
    go (FunctionCall (Function "atan" t@(ComplexType _) _) [arg]) = [replaceWith $ fun t "catanf" [arg]]
    go (FunctionCall (Function "atan" t _) [arg]) = [replaceWith $ fun t "atanf" [arg]]
    go (FunctionCall (Function "acos" t@(ComplexType _) _) [arg]) = [replaceWith $ fun t "cacosf" [arg]]
    go (FunctionCall (Function "acos" t _) [arg]) = [replaceWith $ fun t "acosf" [arg]]
    go (FunctionCall (Function "sinh" t@(ComplexType _) _) [arg]) = [replaceWith $ fun t "csinhf" [arg]]
    go (FunctionCall (Function "sinh" t _) [arg]) = [replaceWith $ fun t "sinhf" [arg]]
    go (FunctionCall (Function "tanh" t@(ComplexType _) _) [arg]) = [replaceWith $ fun t "ctanhf" [arg]]
    go (FunctionCall (Function "tanh" t _) [arg]) = [replaceWith $ fun t "tanhf" [arg]]
    go (FunctionCall (Function "cosh" t@(ComplexType _) _) [arg]) = [replaceWith $ fun t "ccoshf" [arg]]
    go (FunctionCall (Function "cosh" t _) [arg]) = [replaceWith $ fun t "coshf" [arg]]
    go (FunctionCall (Function "asinh" t@(ComplexType _) _) [arg])    = [replaceWith $ fun t "casinhf" [arg]]
    go (FunctionCall (Function "asinh" t _) [arg])    = [replaceWith $ fun t "asinhf" [arg]]
    go (FunctionCall (Function "atanh" t@(ComplexType _) _) [arg])    = [replaceWith $ fun t "catanhf" [arg]]
    go (FunctionCall (Function "atanh" t _) [arg])    = [replaceWith $ fun t "atanhf" [arg]]
    go (FunctionCall (Function "acosh" t@(ComplexType _) _) [arg])    = [replaceWith $ fun t "cacoshf" [arg]]
    go (FunctionCall (Function "acosh" t _) [arg])    = [replaceWith $ fun t "acoshf" [arg]]
    go (FunctionCall (Function "atan2" t@FloatType _) [arg1, arg2]) = [replaceWith $ fun t "atan2f" [arg1, arg2]]
    go (FunctionCall (Function "(.&.)" t _) [arg1, arg2]) = [replaceWith $ binop t "&" arg1 arg2]
    go (FunctionCall (Function "(.|.)" t _) [arg1, arg2]) = [replaceWith $ binop t "|" arg1 arg2]
    go (FunctionCall (Function "xor" t _) [arg1, arg2])   = [replaceWith $ binop t "^" arg1 arg2]
    go (FunctionCall (Function "complement" t _) [arg])   = [replaceWith $ fun t "~" [arg]]
    go (FunctionCall (Function "bit" t _) [arg])  = [replaceWith $ binop t "<<" (litI t 1) arg]
    go (FunctionCall (Function "setBit" t _) [arg1, arg2])    = [replaceWith $ fun t (extend c99 "setBit" t) [arg1, arg2]]
    go (FunctionCall (Function "clearBit" t _) [arg1, arg2])  = [replaceWith $ fun t (extend c99 "clearBit" t) [arg1, arg2]]
    go (FunctionCall (Function "complementBit" t _) [arg1, arg2]) = [replaceWith $ fun t (extend c99 "complementBit" t) [arg1, arg2]]
    go (FunctionCall (Function "testBit" t _) [arg1, arg2])   = [replaceWith $ fun t (extend c99 "testBit" $ typeof arg1) [arg1, arg2]]
    go (FunctionCall (Function "shiftL" t _) [arg1, arg2])    = [replaceWith $ binop t "<<" arg1 arg2]
    go (FunctionCall (Function "shiftLU" t _) [arg1, arg2])    = [replaceWith $ binop t "<<" arg1 arg2]
    go (FunctionCall (Function "shiftR" t _) [arg1, arg2])    = [replaceWith $ binop t ">>" arg1 arg2]
    go (FunctionCall (Function "shiftRU" t _) [arg1, arg2])    = [replaceWith $ binop t ">>" arg1 arg2]
    go (FunctionCall (Function "rotateL" t _) [arg1, arg2])   = [replaceWith $ fun t (extend c99 "rotateL" t) [arg1, arg2]]
    go (FunctionCall (Function "rotateR" t _) [arg1, arg2])   = [replaceWith $ fun t (extend c99 "rotateR" t) [arg1, arg2]]
    go (FunctionCall (Function "reverseBits" t _) [arg])  = [replaceWith $ fun t (extend c99 "reverseBits" t) [arg]]
    go (FunctionCall (Function "bitScan" t _) [arg])  = [replaceWith $ fun t (extend c99 "bitScan" $ typeof arg) [arg]]
    go (FunctionCall (Function "bitCount" t _) [arg]) = [replaceWith $ fun t (extend c99 "bitCount" $ typeof arg) [arg]]
    go (FunctionCall (Function "bitSize" _ _) [intWidth . typeof -> Just n])  = [replaceWith $ litI (NumType Unsigned S32) n]
    go (FunctionCall (Function "isSigned" _ _) [intSigned . typeof -> Just b])    = [replaceWith $ litB b]
    go (FunctionCall (Function "complex" t _) [arg1, arg2])   = [replaceWith $ fun t (extend c99 "complex" $ typeof arg1) [arg1,arg2]]
    go (FunctionCall (Function "creal" t _) [arg])    = [replaceWith $ fun t "crealf" [arg]]
    go (FunctionCall (Function "cimag" t _) [arg])    = [replaceWith $ fun t "cimagf" [arg]]
    go (FunctionCall (Function "conjugate" t _) [arg])    = [replaceWith $ fun t "conjf" [arg]]
    go (FunctionCall (Function "magnitude" t _) [arg])    = [replaceWith $ fun t "cabsf" [arg]]
    go (FunctionCall (Function "phase" t _) [arg])    = [replaceWith $ fun t "cargf" [arg]]
    go (FunctionCall (Function "mkPolar" t _) [arg1, arg2])   = [replaceWith $ fun t (extend c99 "mkPolar" $ typeof arg1) [arg1, arg2]]
    go (FunctionCall (Function "cis" t _) [arg])  = [replaceWith $ fun t (extend c99 "cis" $ typeof arg) [arg]]
    go (FunctionCall (Function "f2i" t _) [arg])  = [replaceWith $ Cast t $ fun FloatType "truncf" [arg]]
    go (FunctionCall (Function "i2n" (ComplexType t) _) [arg])    = [replaceWith $ fun (ComplexType t) (extend c99 "complex" t) [Cast t arg, litF 0]]
    go (FunctionCall (Function "i2n" t _) [arg])  = [replaceWith $ Cast t arg]
    go (FunctionCall (Function "b2i" t _) [arg])  = [replaceWith $ Cast t arg]
    go (FunctionCall (Function "round" t _) [arg])    = [replaceWith $ Cast t $ fun FloatType "roundf" [arg]]
    go (FunctionCall (Function "ceiling" t _) [arg])  = [replaceWith $ Cast t $ fun FloatType "ceilf" [arg]]
    go (FunctionCall (Function "floor" t _) [arg])    = [replaceWith $ Cast t $ fun FloatType "floorf" [arg]]
    go _ = []

tic64xRules :: [Rule]
tic64xRules = [rule go]
  where
    go (FunctionCall (Function "(==)" t _) [arg1@(typeof -> ComplexType _), arg2])    = [replaceWith $ fun t (extend tic64x "equal" $ typeof arg1) [arg1, arg2]]
    go (FunctionCall (Function "(/=)" t _) [arg1@(typeof -> ComplexType _), arg2])    = [replaceWith $ fun t "!" [fun t (extend tic64x "equal" $ typeof arg1) [arg1, arg2]]]
    go (FunctionCall (Function "abs" t _) [arg@(typeof -> FloatType)]) = [replaceWith $ fun t "_fabs" [arg]]
    go (FunctionCall (Function "abs" t _) [arg@(typeof -> (NumType Signed S32))])  = [replaceWith $ fun t "_abs" [arg]]
    go (FunctionCall (Function "(+)" t _) [arg1@(typeof -> ComplexType _), arg2]) = [replaceWith $ fun t (extend tic64x "add" $ typeof arg1) [arg1, arg2]]
    go (FunctionCall (Function "(-)" t _) [arg1@(typeof -> ComplexType _), arg2]) = [replaceWith $ fun t (extend tic64x "sub" $ typeof arg1) [arg1, arg2]]
    go (FunctionCall (Function "(*)" t _) [arg1@(typeof -> ComplexType _), arg2]) = [replaceWith $ fun t (extend tic64x "mult" $ typeof arg1) [arg1, arg2]]
    go (FunctionCall (Function "(/)" t _) [arg1@(typeof -> ComplexType _), arg2]) = [replaceWith $ fun t (extend tic64x "div" $ typeof arg1) [arg1, arg2]]
    go (FunctionCall (Function "exp" t _) [arg1@(typeof -> ComplexType _), arg2]) = [replaceWith $ fun t (extend tic64x "exp" $ typeof arg1) [arg1, arg2]]
    go (FunctionCall (Function "sqrt" t _ ) [arg1@(typeof -> ComplexType _), arg2])    = [replaceWith $ fun t (extend tic64x "sqrt" $ typeof arg1) [arg1, arg2]]
    go (FunctionCall (Function "log" t _) [arg1@(typeof -> ComplexType _), arg2]) = [replaceWith $ fun t (extend tic64x "log" $ typeof arg1) [arg1, arg2]]
    go (FunctionCall (Function "(**)" t _) [arg1@(typeof -> ComplexType _), arg2])    = [replaceWith $ fun t (extend tic64x "cpow" $ typeof arg1) [arg1, arg2]]
    go (FunctionCall (Function "logBase" t _) [arg1@(typeof -> ComplexType _), arg2]) = [replaceWith $ fun t (extend tic64x "logBase" $ typeof arg1) [arg1, arg2]]
    go (FunctionCall (Function fn t _) [arg@(typeof -> ComplexType _)])
       | fn `elem` ["sin","tan","cos","asin","atan","acos","sinh","tanh","cosh","asinh","atanh","acosh","creal","cimag","conjugate","magnitude","phase"]
       = [replaceWith $ fun t (extend tic64x fn $ typeof arg) [arg]]
    go (FunctionCall (Function "rotateL" t _) [arg1@(typeof -> (NumType Unsigned S32)), arg2])   = [replaceWith $ fun t "_rotl" [arg1, arg2]]
    go (FunctionCall (Function "reverseBits" t _) [arg@(typeof -> (NumType Unsigned S32))])  = [replaceWith $ fun t "_bitr" [arg]]
    go (FunctionCall (Function "bitCount" t _) [arg@(typeof -> (NumType Unsigned S32))])  = [replaceWith $ fun t "_dotpu4" [fun t "_bitc4" [arg], litI32 0x01010101]]
    go (FunctionCall (Function _ t _) [arg@(typeof -> ComplexType _)]) = [replaceWith $ fun t (extend tic64x "creal" $ typeof arg) [arg]]
    go _ = []

traceRules :: [Rule]
traceRules = [rule trace]
  where
    trace (FunctionCall (Function "trace" t _) [lab, val]) = [WithId acts]
      where
       acts i = [replaceWith trcVar, propagate decl, propagate trc, propagate frame]
         where
            v          = Variable t trcVarName
            trcVar     = varToExpr v
            trcVarName = "trc" ++ show i
            defTrcVar  = Declaration v Nothing
            decl :: Block () -> [Action (Block ())]
            decl (Block defs prg) = [replaceWith $ Block (defs ++ [defTrcVar]) prg]
            trc :: Program () -> [Action (Program ())]
            trc instr = [replaceWith $ Sequence [Assign trcVar val,trcCall,instr]]
            trcCall = call (extend' "trace" t) [ValueParameter trcVar, ValueParameter lab]
            frame (Proc pname ins outs (Just prg)) = [replaceWith $ Proc pname ins outs prg']
              where
                prg' = Just $ case prg of
                    Block _ (Sequence (ProcedureCall "traceStart" [] : _)) -> prg
                    Block ds ps -> Block ds (Sequence [call "traceStart" [], ps, call "traceEnd" []])
    trace _ = []

extend :: Platform -> String -> Type -> String
extend Platform{..} s t = s ++ "_fun_" ++ fromMaybe (show t) (lookup t types)

extend' :: String -> Type -> String
extend' s t = s ++ "_" ++ show t

log2 :: Integer -> Maybe Integer
log2 n
    | n == 2 Prelude.^ l = Just l
    | otherwise          = Nothing
  where
    l = toInteger $ length $ takeWhile (<n) $ iterate (*2) 1

first, second :: String
first  = "member1"
second = "member2"
