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
    , tic64x
    , c99Rules
    , tic64xRules
    , traceRules
    , nativeArrayRules
    ) where


import Feldspar.Range
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Imperative.Representation hiding (In, Out, Block)
import Feldspar.Compiler.Imperative.Frontend

availablePlatforms :: [Platform]
availablePlatforms = [ c99, tic64x ]

c99 :: Platform
c99 = Platform {
    name = "c99",
    types =
        [ (NumType Signed S8,    "int8_t",   "int8")
        , (NumType Signed S16,   "int16_t",  "int16")
        , (NumType Signed S32,   "int32_t",  "int32")
        , (NumType Signed S64,   "int64_t",  "int64")
        , (NumType Unsigned S8,  "uint8_t",  "uint8")
        , (NumType Unsigned S16, "uint16_t", "uint16")
        , (NumType Unsigned S32, "uint32_t", "uint32")
        , (NumType Unsigned S64, "uint64_t", "uint64")
        , (BoolType,  "uint32_t",    "uint32_t") -- TODO sizeof(bool) is implementation dependent
        , (FloatType, "float",  "float")
        , (ComplexType FloatType,              "float complex",    "complexOf_float")
        ] ,
    values =
        [ (ComplexType FloatType, \cx -> "(" ++ showRe cx ++ "+" ++ showIm cx ++ "i)")
        , (BoolType, \b -> if boolValue b then "true" else "false")
        ] ,
    includes =
        [ "\"feldspar_c99.h\""
        , "\"feldspar_array.h\""
        , "\"feldspar_future.h\""
        , "\"ivar.h\""
        , "\"taskpool.h\""
        , "<stdint.h>"
        , "<string.h>"
        , "<math.h>"
        , "<stdbool.h>"
        , "<complex.h>"],
    platformRules = c99Rules ++ traceRules ++ arrayRules,
    isRestrict = NoRestrict
}

tic64x :: Platform
tic64x = Platform {
    name = "tic64x",
    types =
        [ (NumType Signed S8,    "char",     "char")
        , (NumType Signed S16,   "short",    "short")
        , (NumType Signed S32,   "int",      "int")
        , (NumType Signed S40,   "long",     "long")
        , (NumType Signed S64,   "long long","llong")
        , (NumType Unsigned S8,  "unsigned char",  "uchar")
        , (NumType Unsigned S16, "unsigned short", "ushort")
        , (NumType Unsigned S32, "unsigned",       "uint")
        , (NumType Unsigned S40, "unsigned long",  "ulong")
        , (NumType Unsigned S64, "unsigned long long", "ullong")
        , (BoolType,  "int",    "bool")
        , (FloatType, "float",  "float")
        , (ComplexType FloatType,              "complexOf_float",  "complexOf_float")
        ] ,
    values = 
        [ (ComplexType FloatType, \cx -> "complex_fun_float(" ++ showRe cx ++ "," ++ showIm cx ++ ")")
        , (BoolType, \b -> if boolValue b then "1" else "0")
        ] ,
    includes = ["\"feldspar_tic64x.h\"", "\"feldspar_array.h\"", "<c6x.h>", "<string.h>", "<math.h>"],
    platformRules = tic64xRules ++ c99Rules ++ traceRules,
    isRestrict = Restrict
}

showRe, showIm :: Constant t -> String
showRe = showConstant . realPartComplexValue
showIm = showConstant . imagPartComplexValue

showConstant :: Constant t -> String
showConstant (IntConst c _) = show c
showConstant (FloatConst c) = show c ++ "f"

arrayRules :: [Rule]
arrayRules = [rule copy]
  where
    copy (Call "copy" _ [Out arg1, In arg2])
        | arg1 == arg2 = [replaceWith $ Skip]
        | not (isArray (typeof arg1)) = [replaceWith $ arg1 := arg2]
    copy (Call "copy" k (dst@(Out arg1):ins'@(in1:ins))) | isArray (typeof arg1)
        = [replaceWith $ Seq ([
               initArray arg1 (foldr ePlus (litI32 0) aLens)
             , Call "copyArray" k [dst, in1]
             ] ++ flattenCopy k dst ins argnLens arg1len)]
           where
             aLens@(arg1len:argnLens) = map (\(In src) -> arrayLength src) ins'
    copy (Call "copy" _ _) = error "Multiple scalar arguments to copy"
    copy _ = []

nativeArrayRules :: [Rule]
nativeArrayRules = [rule toNativeExpr, rule toNativeProg, rule toNativeVariable]
  where
    toNativeExpr :: Expression () -> [Action (Repr (Expression ()))]
    toNativeExpr (ArrayElem arr ix)
      | native (typeof arr) = [replaceWith $ NativeElem arr ix]
    toNativeExpr _ = []

    toNativeProg (Call "initArray" k [Out arr,esz,num])
      | native (typeof arr) = [replaceWith $ Call "assert" k [Out arr]]
    toNativeProg (Call "freeArray" _ [Out arr])
      | native (typeof arr) = [replaceWith $ Skip]
    toNativeProg _ = []


    toNativeVariable :: Variable () -> [Action (Repr (Variable ()))]
    toNativeVariable v@Variable{..} | ArrayType{} <- varType 
      = [replaceWith $ v { varType = nativeArray varType}]
    toNativeVariable _ = []

    nativeArray (ArrayType sz t) = NativeArray (fromSingleton sz) (nativeArray t)
    nativeArray t = t

    native (NativeArray {}) = True
    native _                = False

    fromSingleton r = if isSingleton r
                        then Just $ upperBound r
                        else Nothing

flattenCopy :: Kind -> Param -> [Param] -> [Expression ()] -> Expression () -> [Prog]
flattenCopy _ _ [] [] _ = []
flattenCopy k dst (t:ts) (l:ls) cLen =
  (Call "copyArrayPos" k [dst, In cLen, t]):flattenCopy k dst ts ls (ePlus cLen l)

ePlus :: Expression () -> Expression () -> Expression ()
ePlus (ConstExpr (IntConst 0 _)) e = e
ePlus e (ConstExpr (IntConst 0 _)) = e
ePlus e1 e2 = binop (NumType Signed S32) "+" e1 e2

c99Rules :: [Rule]
c99Rules = [rule c99]
  where
    c99 :: Expression () -> [Action (Repr (Expression ()))]
    c99 (FunctionCall (Function "(!)" _ _) [arg1,arg2])    = [replaceWith $ ArrayElem arg1 arg2]
    c99 (FunctionCall (Function "getFst" _ _) [arg]) = [replaceWith $ StructField arg first]
    c99 (FunctionCall (Function "getSnd" _ _) [arg]) = [replaceWith $ StructField arg second]
    c99 (FunctionCall (Function "(==)" t _) [arg1, arg2])  = [replaceWith $ binop t "==" arg1 arg2]
    c99 (FunctionCall (Function "(/=)" t _) [arg1, arg2])  = [replaceWith $ binop t "!=" arg1 arg2]
    c99 (FunctionCall (Function "(<)" t _) [arg1, arg2])   = [replaceWith $ binop t "<" arg1 arg2]
    c99 (FunctionCall (Function "(>)" t _) [arg1, arg2])   = [replaceWith $ binop t ">" arg1 arg2]
    c99 (FunctionCall (Function "(<=)" t _) [arg1, arg2])  = [replaceWith $ binop t "<=" arg1 arg2]
    c99 (FunctionCall (Function "(>=)" t _) [arg1, arg2])  = [replaceWith $ binop t ">=" arg1 arg2]
    c99 (FunctionCall (Function "not" t _) [arg])  = [replaceWith $ fun t "!" [arg]]
    c99 (FunctionCall (Function "(&&)" t _) [arg1, arg2])  = [replaceWith $ binop t "&&" arg1 arg2]
    c99 (FunctionCall (Function "(||)" t _) [arg1, arg2])  = [replaceWith $ binop t "||" arg1 arg2]
    c99 (FunctionCall (Function "quot" t _) [arg1, arg2])  = [replaceWith $ binop t "/" arg1 arg2]
    c99 (FunctionCall (Function "rem" t _) [arg1, arg2])   = [replaceWith $ binop t "%" arg1 arg2]
    c99 (FunctionCall (Function "(^)" t _) [arg1, arg2])   = [replaceWith $ fun t (extend "pow" t) [arg1, arg2]]
    c99 (FunctionCall (Function "abs" t _) [arg])  = [replaceWith $ fun t (extend "abs" t) [arg]]
    c99 (FunctionCall (Function "signum" t _) [arg])   = [replaceWith $ fun t (extend "signum" t) [arg]]
    c99 (FunctionCall (Function "(+)" t _) [arg1, arg2])   = [replaceWith $ binop t "+" arg1 arg2]
    c99 (FunctionCall (Function "(-)" t _) [ConstExpr (IntConst 0 _), arg2]) = [replaceWith $ fun t "-" [arg2]]
    c99 (FunctionCall (Function "(-)" t _) [ConstExpr (FloatConst 0), arg2]) = [replaceWith $ fun t "-" [arg2]]
    c99 (FunctionCall (Function "(-)" t _) [arg1, arg2])   = [replaceWith $ binop t "-" arg1 arg2]
    c99 (FunctionCall (Function "(*)" t _) [ConstExpr (IntConst (log2 -> Just n) _), arg2])    = [replaceWith $ binop t "<<" arg2 (litI32 n)]
    c99 (FunctionCall (Function "(*)" t _) [arg1, ConstExpr (IntConst (log2 -> Just n) _)])    = [replaceWith $ binop t "<<" arg1 (litI32 n)]
    c99 (FunctionCall (Function "(*)" t _) [arg1, arg2])   = [replaceWith $ binop t "*" arg1 arg2]
    c99 (FunctionCall (Function "(/)" t _) [arg1, arg2])   = [replaceWith $ binop t "/" arg1 arg2]
    c99 (FunctionCall (Function "div" t _) [arg1, arg2]) =
        [replaceWith $ StructField (fun div_t (div t) [arg1, arg2]) "quot"]
      where div_t = Alias (StructType [("quot", t), ("rem", t)]) "div_t"
            div (NumType Signed S8)  = "div"
            div (NumType Signed S16) = "div"
            div (NumType Signed S32) = "div"
            div (NumType Signed S40) = "ldiv"
            div (NumType Signed S64) = "lldiv"
    c99 (FunctionCall (Function "exp" t@(ComplexType _) _) [arg])  = [replaceWith $ fun t "cexpf" [arg]]
    c99 (FunctionCall (Function "exp" t _) [arg])  = [replaceWith $ fun t "expf" [arg]]
    c99 (FunctionCall (Function "sqrt" t@(ComplexType _) _) [arg]) = [replaceWith $ fun t "csqrtf" [arg]]
    c99 (FunctionCall (Function "sqrt" t _) [arg]) = [replaceWith $ fun t "sqrtf" [arg]]
    c99 (FunctionCall (Function "log" t@(ComplexType _) _) [arg])  = [replaceWith $ fun t "clogf" [arg]]
    c99 (FunctionCall (Function "log" t _) [arg])  = [replaceWith $ fun t "logf" [arg]]
    c99 (FunctionCall (Function "(**)" t@(ComplexType _) _) [arg1, arg2])  = [replaceWith $ fun t "cpowf" [arg1,arg2]]
    c99 (FunctionCall (Function "(**)" t _) [arg1, arg2])  = [replaceWith $ fun t "powf" [arg1,arg2]]
    c99 (FunctionCall (Function "logBase" t _) [arg1, arg2])   = [replaceWith $ fun t (extend "logBase" t) [arg1,arg2]]
    c99 (FunctionCall (Function "sin" t@(ComplexType _) _) [arg])  = [replaceWith $ fun t "csinf" [arg]]
    c99 (FunctionCall (Function "sin" t _) [arg])  = [replaceWith $ fun t "sinf" [arg]]
    c99 (FunctionCall (Function "tan" t@(ComplexType _) _) [arg])  = [replaceWith $ fun t "ctanf" [arg]]
    c99 (FunctionCall (Function "tan" t _) [arg])  = [replaceWith $ fun t "tanf" [arg]]
    c99 (FunctionCall (Function "cos" t@(ComplexType _)  _) [arg])  = [replaceWith $ fun t "ccosf" [arg]]
    c99 (FunctionCall (Function "cos" t _) [arg])  = [replaceWith $ fun t "cosf" [arg]]
    c99 (FunctionCall (Function "asin" t@(ComplexType _) _) [arg]) = [replaceWith $ fun t "casinf" [arg]]
    c99 (FunctionCall (Function "asin" t _) [arg]) = [replaceWith $ fun t "asinf" [arg]]
    c99 (FunctionCall (Function "atan" t@(ComplexType _) _) [arg]) = [replaceWith $ fun t "catanf" [arg]]
    c99 (FunctionCall (Function "atan" t _) [arg]) = [replaceWith $ fun t "atanf" [arg]]
    c99 (FunctionCall (Function "acos" t@(ComplexType _) _) [arg]) = [replaceWith $ fun t "cacosf" [arg]]
    c99 (FunctionCall (Function "acos" t _) [arg]) = [replaceWith $ fun t "acosf" [arg]]
    c99 (FunctionCall (Function "sinh" t@(ComplexType _) _) [arg]) = [replaceWith $ fun t "csinhf" [arg]]
    c99 (FunctionCall (Function "sinh" t _) [arg]) = [replaceWith $ fun t "sinhf" [arg]]
    c99 (FunctionCall (Function "tanh" t@(ComplexType _) _) [arg]) = [replaceWith $ fun t "ctanhf" [arg]]
    c99 (FunctionCall (Function "tanh" t _) [arg]) = [replaceWith $ fun t "tanhf" [arg]]
    c99 (FunctionCall (Function "cosh" t@(ComplexType _) _) [arg]) = [replaceWith $ fun t "ccoshf" [arg]]
    c99 (FunctionCall (Function "cosh" t _) [arg]) = [replaceWith $ fun t "coshf" [arg]]
    c99 (FunctionCall (Function "asinh" t@(ComplexType _) _) [arg])    = [replaceWith $ fun t "casinhf" [arg]]
    c99 (FunctionCall (Function "asinh" t _) [arg])    = [replaceWith $ fun t "asinhf" [arg]]
    c99 (FunctionCall (Function "atanh" t@(ComplexType _) _) [arg])    = [replaceWith $ fun t "catanhf" [arg]]
    c99 (FunctionCall (Function "atanh" t _) [arg])    = [replaceWith $ fun t "atanhf" [arg]]
    c99 (FunctionCall (Function "acosh" t@(ComplexType _) _) [arg])    = [replaceWith $ fun t "cacoshf" [arg]]
    c99 (FunctionCall (Function "acosh" t _) [arg])    = [replaceWith $ fun t "acoshf" [arg]]
    c99 (FunctionCall (Function "(.&.)" t _) [arg1, arg2]) = [replaceWith $ binop t "&" arg1 arg2]
    c99 (FunctionCall (Function "(.|.)" t _) [arg1, arg2]) = [replaceWith $ binop t "|" arg1 arg2]
    c99 (FunctionCall (Function "xor" t _) [arg1, arg2])   = [replaceWith $ binop t "^" arg1 arg2]
    c99 (FunctionCall (Function "complement" t _) [arg])   = [replaceWith $ fun t "~" [arg]]
    c99 (FunctionCall (Function "bit" t _) [arg])  = [replaceWith $ binop t "<<" (litI t 1) arg]
    c99 (FunctionCall (Function "setBit" t _) [arg1, arg2])    = [replaceWith $ fun t (extend "setBit" t) [arg1, arg2]]
    c99 (FunctionCall (Function "clearBit" t _) [arg1, arg2])  = [replaceWith $ fun t (extend "clearBit" t) [arg1, arg2]]
    c99 (FunctionCall (Function "complementBit" t _) [arg1, arg2]) = [replaceWith $ fun t (extend "complementBit" t) [arg1, arg2]]
    c99 (FunctionCall (Function "testBit" t _) [arg1, arg2])   = [replaceWith $ fun t (extend "testBit" $ typeof arg1) [arg1, arg2]]
    c99 (FunctionCall (Function "shiftL" t _) [arg1, arg2])    = [replaceWith $ binop t "<<" arg1 arg2]
    c99 (FunctionCall (Function "shiftR" t _) [arg1, arg2])    = [replaceWith $ binop t ">>" arg1 arg2]
    c99 (FunctionCall (Function "rotateL" t _) [arg1, arg2])   = [replaceWith $ fun t (extend "rotateL" t) [arg1, arg2]]
    c99 (FunctionCall (Function "rotateR" t _) [arg1, arg2])   = [replaceWith $ fun t (extend "rotateR" t) [arg1, arg2]]
    c99 (FunctionCall (Function "reverseBits" t _) [arg])  = [replaceWith $ fun t (extend "reverseBits" t) [arg]]
    c99 (FunctionCall (Function "bitScan" t _) [arg])  = [replaceWith $ fun t (extend "bitScan" $ typeof arg) [arg]]
    c99 (FunctionCall (Function "bitCount" t _) [arg]) = [replaceWith $ fun t (extend "bitCount" $ typeof arg) [arg]]
    c99 (FunctionCall (Function "bitSize" _ _) [intWidth . typeof -> Just n])  = [replaceWith $ litI (NumType Unsigned S32) n]
    c99 (FunctionCall (Function "isSigned" _ _) [intSigned . typeof -> Just b])    = [replaceWith $ litB b]
    c99 (FunctionCall (Function "complex" t _) [arg1, arg2])   = [replaceWith $ fun t (extend "complex" $ typeof arg1) [arg1,arg2]]
    c99 (FunctionCall (Function "creal" t _) [arg])    = [replaceWith $ fun t "crealf" [arg]]
    c99 (FunctionCall (Function "cimag" t _) [arg])    = [replaceWith $ fun t "cimagf" [arg]]
    c99 (FunctionCall (Function "conjugate" t _) [arg])    = [replaceWith $ fun t "conjf" [arg]]
    c99 (FunctionCall (Function "magnitude" t _) [arg])    = [replaceWith $ fun t "cabsf" [arg]]
    c99 (FunctionCall (Function "phase" t _) [arg])    = [replaceWith $ fun t "cargf" [arg]]
    c99 (FunctionCall (Function "mkPolar" t _) [arg1, arg2])   = [replaceWith $ fun t (extend "mkPolar" $ typeof arg1) [arg1, arg2]]
    c99 (FunctionCall (Function "cis" t _) [arg])  = [replaceWith $ fun t (extend "cis" $ typeof arg) [arg]]
    c99 (FunctionCall (Function "f2i" t _) [arg])  = [replaceWith $ Cast t $ fun FloatType "truncf" [arg]]
    c99 (FunctionCall (Function "i2n" (ComplexType t) _) [arg])    = [replaceWith $ fun (ComplexType t) (extend "complex" t) [Cast t arg, litF 0]]
    c99 (FunctionCall (Function "i2n" t _) [arg])  = [replaceWith $ Cast t arg]
    c99 (FunctionCall (Function "b2i" t _) [arg])  = [replaceWith $ Cast t arg]
    c99 (FunctionCall (Function "round" t _) [arg])    = [replaceWith $ Cast t $ fun FloatType "roundf" [arg]]
    c99 (FunctionCall (Function "ceiling" t _) [arg])  = [replaceWith $ Cast t $ fun FloatType "ceilf" [arg]]
    c99 (FunctionCall (Function "floor" t _) [arg])    = [replaceWith $ Cast t $ fun FloatType "floorf" [arg]]
    c99 _ = []

tic64xRules :: [Rule]
tic64xRules = [rule tic64x]
  where
    tic64x (FunctionCall (Function "(==)" t _) [arg1@(typeof -> ComplexType _), arg2])    = [replaceWith $ fun t (extend "equal" $ typeof arg1) [arg1, arg2]]
    tic64x (FunctionCall (Function "(/=)" t _) [arg1@(typeof -> ComplexType _), arg2])    = [replaceWith $ fun t "!" [fun t (extend "equal" $ typeof arg1) [arg1, arg2]]]
    tic64x (FunctionCall (Function "abs" t _) [arg@(typeof -> FloatType)]) = [replaceWith $ fun t "_fabs" [arg]]
    tic64x (FunctionCall (Function "abs" t _) [arg@(typeof -> (NumType Signed S32))])  = [replaceWith $ fun t "_abs" [arg]]
    tic64x (FunctionCall (Function "(+)" t _) [arg1@(typeof -> ComplexType _), arg2]) = [replaceWith $ fun t (extend "add" $ typeof arg1) [arg1, arg2]]
    tic64x (FunctionCall (Function "(-)" t _) [arg1@(typeof -> ComplexType _), arg2]) = [replaceWith $ fun t (extend "sub" $ typeof arg1) [arg1, arg2]]
    tic64x (FunctionCall (Function "(*)" t _) [arg1@(typeof -> ComplexType _), arg2]) = [replaceWith $ fun t (extend "mult" $ typeof arg1) [arg1, arg2]]
    tic64x (FunctionCall (Function "(/)" t _) [arg1@(typeof -> ComplexType _), arg2]) = [replaceWith $ fun t (extend "div" $ typeof arg1) [arg1, arg2]]
    tic64x (FunctionCall (Function "exp" t _) [arg1@(typeof -> ComplexType _), arg2]) = [replaceWith $ fun t (extend "exp" $ typeof arg1) [arg1, arg2]]
    tic64x (FunctionCall (Function "sqrt" t _ ) [arg1@(typeof -> ComplexType _), arg2])    = [replaceWith $ fun t (extend "sqrt" $ typeof arg1) [arg1, arg2]]
    tic64x (FunctionCall (Function "log" t _) [arg1@(typeof -> ComplexType _), arg2]) = [replaceWith $ fun t (extend "log" $ typeof arg1) [arg1, arg2]]
    tic64x (FunctionCall (Function "(**)" t _) [arg1@(typeof -> ComplexType _), arg2])    = [replaceWith $ fun t (extend "cpow" $ typeof arg1) [arg1, arg2]]
    tic64x (FunctionCall (Function "logBase" t _) [arg1@(typeof -> ComplexType _), arg2]) = [replaceWith $ fun t (extend "logBase" $ typeof arg1) [arg1, arg2]]
    tic64x (FunctionCall (Function fn t _) [arg@(typeof -> ComplexType _)])
        | fn `elem` ["sin","tan","cos","asin","atan","acos","sinh","tanh","cosh","asinh","atanh","acosh","creal","cimag","conjugate","magnitude","phase"]
            = [replaceWith $ fun t (extend fn $ typeof arg) [arg]]
    tic64x (FunctionCall (Function "rotateL" t _) [arg1@(typeof -> (NumType Unsigned S32)), arg2])   = [replaceWith $ fun t "_rotl" [arg1, arg2]]
    tic64x (FunctionCall (Function "reverseBits" t _) [arg@(typeof -> (NumType Unsigned S32))])  = [replaceWith $ fun t "_bitr" [arg]]
    tic64x (FunctionCall (Function "bitCount" t _) [arg@(typeof -> (NumType Unsigned S32))])  = [replaceWith $ fun t "_dotpu4" [fun t "_bitc4" [arg], litI (NumType Unsigned S32) 0x01010101]]
    tic64x (FunctionCall (Function _ t _) [arg@(typeof -> ComplexType _)]) = [replaceWith $ fun t (extend "creal" $ typeof arg) [arg]]
    tic64x _ = []

traceRules :: [Rule]
traceRules = [rule trace]
  where
    trace :: Expression () -> [Action (Repr (Expression ()))]
    trace (FunctionCall (Function "trace" t _) [lab, val]) = [WithId acts]
      where
       acts i = [replaceWith trcVar, propagate decl, propagate trc, propagate frame]
         where
            v          = Variable Value t trcVarName
            trcVar     = varToExpr v
            trcVarName = "trc" ++ show i
            defTrcVar  = Declaration v Nothing
            decl (Bl defs prg) = [replaceWith $ Bl (defs ++ [defTrcVar]) prg]
            trc :: Prog -> [Action (Repr Prog)]
            trc instr = [replaceWith $ Seq [trcVar := val,trcCall,instr]]
            trcCall = Call (extend' "trace" t) KTrace [In trcVar, In lab]
            frame (ProcDf pname knd ins outs prg) = [replaceWith $ ProcDf pname knd ins outs prg']
              where
                prg' = case prg of
                    Seq (Call "traceStart" _ [] : _) -> prg
                    Block _ (Seq (Call "traceStart" _ [] : _)) -> prg
                    _ -> Seq [Call "traceStart" KTrace [], prg, Call "traceEnd" KTrace []]
    trace _ = []

extend :: String -> Type -> String
extend s t = s ++ "_fun_" ++ show t

extend' :: String -> Type -> String
extend' s t = s ++ "_" ++ show t

log2 :: Integer -> Maybe Integer
log2 n
    | n == 2 Prelude.^ l = Just l
    | otherwise          = Nothing
  where
    l = toInteger $ length $ takeWhile (<=n) $ map (2 Prelude.^) [1..]

first, second :: String
first  = "member1"
second = "member2"
