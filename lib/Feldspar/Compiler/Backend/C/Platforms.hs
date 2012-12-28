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
import Feldspar.Compiler.Imperative.Representation hiding (Alias, Type, Cast, In, Out, Block, NativeArray, NativeElem, Variable, Pointer)
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
showConstant (IntConst c _ _ _) = show c
showConstant (FloatConst c _ _) = show c ++ "f"

arrayRules :: [Rule]
arrayRules = [rule copy]
  where
    copy (Call "copy" [Out arg1, In arg2])
        | arg1 == arg2 = [replaceWith $ Skip]
        | not (isArray (typeof arg1)) = [replaceWith $ arg1 := arg2]
    copy (Call "copy" (dst@(Out arg1):ins'@(in1:ins))) | isArray (typeof arg1)
        = [replaceWith $ Seq ([
               initArray arg1 (foldr ePlus (litI32 0) aLens)
             , Call "copyArray" [dst, in1]
             ] ++ flattenCopy dst ins argnLens arg1len)]
           where
             aLens@(arg1len:argnLens) = map (\(In src) -> arrayLength src) ins'
    copy (Call "copy" _) = error "Multiple scalar arguments to copy"
    copy _ = []

nativeArrayRules :: [Rule]
nativeArrayRules = [rule toNativeExpr, rule toNativeProg, rule toNativeVariable]
  where
    toNativeExpr (arr :!: ix)
      | native (typeof arr) = [replaceWith $ NativeElem arr ix]
    toNativeExpr _ = []

    toNativeProg (Call "initArray" [Out arr,esz,num])
      | native (typeof arr) = [replaceWith $ Call "assert" [Out arr]]
    toNativeProg (Call "freeArray" [Out arr])
      | native (typeof arr) = [replaceWith $ Skip]
    toNativeProg _ = []

    toNativeVariable (Pointer arr@(SizedArray sz t) n)
      = [replaceWith $ Pointer (nativeArray arr) n]
    toNativeVariable (Variable arr@(SizedArray sz t) n)
      = [replaceWith $ Variable (nativeArray arr) n]
    toNativeVariable _ = []

    nativeArray (SizedArray sz t) = NativeArray (fromSingleton sz) (nativeArray t)
    nativeArray t = t

    native (NativeArray {}) = True
    native _                = False

    fromSingleton r = if isSingleton r
                        then Just $ upperBound r
                        else Nothing

flattenCopy :: Param -> [Param] -> [Expr] -> Expr -> [Prog]
flattenCopy _ [] [] _ = []
flattenCopy dst (t:ts) (l:ls) cLen =
  (Call "copyArrayPos" [dst, In cLen, t]):flattenCopy dst ts ls (ePlus cLen l)

ePlus :: Expr -> Expr -> Expr
ePlus e1 e2 = Binop I32 "+" [e1, e2]

c99Rules :: [Rule]
c99Rules = [rule c99]
  where
    c99 (Fun _ "(!)" [arg1,arg2])    = [replaceWith $ arg1 :!: arg2]
    c99 (Fun _ "getFst" [arg]) = [replaceWith $ arg :.: first]
    c99 (Fun _ "getSnd" [arg]) = [replaceWith $ arg :.: second]
    c99 (Fun t "(==)" [arg1, arg2])  = [replaceWith $ Binop t "==" [arg1, arg2]]
    c99 (Fun t "(/=)" [arg1, arg2])  = [replaceWith $ Binop t "!=" [arg1, arg2]]
    c99 (Fun t "(<)" [arg1, arg2])   = [replaceWith $ Binop t "<" [arg1, arg2]]
    c99 (Fun t "(>)" [arg1, arg2])   = [replaceWith $ Binop t ">" [arg1, arg2]]
    c99 (Fun t "(<=)" [arg1, arg2])  = [replaceWith $ Binop t "<=" [arg1, arg2]]
    c99 (Fun t "(>=)" [arg1, arg2])  = [replaceWith $ Binop t ">=" [arg1, arg2]]
    c99 (Fun t "not" [arg])  = [replaceWith $ Fun t "!" [arg]]
    c99 (Fun t "(&&)" [arg1, arg2])  = [replaceWith $ Binop t "&&" [arg1, arg2]]
    c99 (Fun t "(||)" [arg1, arg2])  = [replaceWith $ Binop t "||" [arg1, arg2]]
    c99 (Fun t "quot" [arg1, arg2])  = [replaceWith $ Binop t "/" [arg1, arg2]]
    c99 (Fun t "rem" [arg1, arg2])   = [replaceWith $ Binop t "%" [arg1, arg2]]
    c99 (Fun t "(^)" [arg1, arg2])   = [replaceWith $ Fun t (extend "pow" t) [arg1, arg2]]
    c99 (Fun t "abs" [arg])  = [replaceWith $ Fun t (extend "abs" t) [arg]]
    c99 (Fun t "signum" [arg])   = [replaceWith $ Fun t (extend "signum" t) [arg]]
    c99 (Fun t "(+)" [arg1, arg2])   = [replaceWith $ Binop t "+" [arg1, arg2]]
    c99 (Fun t "(-)" [LitI _ 0, arg2])   = [replaceWith $ Fun t "-" [arg2]]
    c99 (Fun t "(-)" [LitF 0, arg2]) = [replaceWith $ Fun t "-" [arg2]]
    c99 (Fun t "(-)" [arg1, arg2])   = [replaceWith $ Binop t "-" [arg1, arg2]]
    c99 (Fun t "(*)" [LitI _ (log2 -> Just n), arg2])    = [replaceWith $ Binop t "<<" [arg2, LitI I32 n]]
    c99 (Fun t "(*)" [arg1, LitI _ (log2 -> Just n)])    = [replaceWith $ Binop t "<<" [arg1, LitI I32 n]]
    c99 (Fun t "(*)" [arg1, arg2])   = [replaceWith $ Binop t "*" [arg1, arg2]]
    c99 (Fun t "(/)" [arg1, arg2])   = [replaceWith $ Binop t "/" [arg1, arg2]]
    c99 (Fun t "div" [arg1, arg2]) =
        [replaceWith $ Fun div_t (div t) [arg1, arg2] :.: "quot"]
      where div_t = Alias (Struct [("quot", t), ("rem", t)]) "div_t"
            div I8  = "div"
            div I16 = "div"
            div I32 = "div"
            div I40 = "ldiv"
            div I64 = "lldiv"
    c99 (Fun t@(Complex _) "exp" [arg])  = [replaceWith $ Fun t "cexpf" [arg]]
    c99 (Fun t "exp" [arg])  = [replaceWith $ Fun t "expf" [arg]]
    c99 (Fun t@(Complex _) "sqrt" [arg]) = [replaceWith $ Fun t "csqrtf" [arg]]
    c99 (Fun t "sqrt" [arg]) = [replaceWith $ Fun t "sqrtf" [arg]]
    c99 (Fun t@(Complex _) "log" [arg])  = [replaceWith $ Fun t "clogf" [arg]]
    c99 (Fun t "log" [arg])  = [replaceWith $ Fun t "logf" [arg]]
    c99 (Fun t@(Complex _) "(**)" [arg1, arg2])  = [replaceWith $ Fun t "cpowf" [arg1,arg2]]
    c99 (Fun t "(**)" [arg1, arg2])  = [replaceWith $ Fun t "powf" [arg1,arg2]]
    c99 (Fun t "logBase" [arg1, arg2])   = [replaceWith $ Fun t (extend "logBase" t) [arg1,arg2]]
    c99 (Fun t@(Complex _) "sin" [arg])  = [replaceWith $ Fun t "csinf" [arg]]
    c99 (Fun t "sin" [arg])  = [replaceWith $ Fun t "sinf" [arg]]
    c99 (Fun t@(Complex _) "tan" [arg])  = [replaceWith $ Fun t "ctanf" [arg]]
    c99 (Fun t "tan" [arg])  = [replaceWith $ Fun t "tanf" [arg]]
    c99 (Fun t@(Complex _) "cos" [arg])  = [replaceWith $ Fun t "ccosf" [arg]]
    c99 (Fun t "cos" [arg])  = [replaceWith $ Fun t "cosf" [arg]]
    c99 (Fun t@(Complex _) "asin" [arg]) = [replaceWith $ Fun t "casinf" [arg]]
    c99 (Fun t "asin" [arg]) = [replaceWith $ Fun t "asinf" [arg]]
    c99 (Fun t@(Complex _) "atan" [arg]) = [replaceWith $ Fun t "catanf" [arg]]
    c99 (Fun t "atan" [arg]) = [replaceWith $ Fun t "atanf" [arg]]
    c99 (Fun t@(Complex _) "acos" [arg]) = [replaceWith $ Fun t "cacosf" [arg]]
    c99 (Fun t "acos" [arg]) = [replaceWith $ Fun t "acosf" [arg]]
    c99 (Fun t@(Complex _) "sinh" [arg]) = [replaceWith $ Fun t "csinhf" [arg]]
    c99 (Fun t "sinh" [arg]) = [replaceWith $ Fun t "sinhf" [arg]]
    c99 (Fun t@(Complex _) "tanh" [arg]) = [replaceWith $ Fun t "ctanhf" [arg]]
    c99 (Fun t "tanh" [arg]) = [replaceWith $ Fun t "tanhf" [arg]]
    c99 (Fun t@(Complex _) "cosh" [arg]) = [replaceWith $ Fun t "ccoshf" [arg]]
    c99 (Fun t "cosh" [arg]) = [replaceWith $ Fun t "coshf" [arg]]
    c99 (Fun t@(Complex _) "asinh" [arg])    = [replaceWith $ Fun t "casinhf" [arg]]
    c99 (Fun t "asinh" [arg])    = [replaceWith $ Fun t "asinhf" [arg]]
    c99 (Fun t@(Complex _) "atanh" [arg])    = [replaceWith $ Fun t "catanhf" [arg]]
    c99 (Fun t "atanh" [arg])    = [replaceWith $ Fun t "atanhf" [arg]]
    c99 (Fun t@(Complex _) "acosh" [arg])    = [replaceWith $ Fun t "cacoshf" [arg]]
    c99 (Fun t "acosh" [arg])    = [replaceWith $ Fun t "acoshf" [arg]]
    c99 (Fun t "(.&.)" [arg1, arg2]) = [replaceWith $ Binop t "&" [arg1, arg2]]
    c99 (Fun t "(.|.)" [arg1, arg2]) = [replaceWith $ Binop t "|" [arg1, arg2]]
    c99 (Fun t "xor" [arg1, arg2])   = [replaceWith $ Binop t "^" [arg1, arg2]]
    c99 (Fun t "complement" [arg])   = [replaceWith $ Fun t "~" [arg]]
    c99 (Fun t "bit" [arg])  = [replaceWith $ Binop t "<<" [LitI t 1, arg]]
    c99 (Fun t "setBit" [arg1, arg2])    = [replaceWith $ Fun t (extend "setBit" t) [arg1, arg2]]
    c99 (Fun t "clearBit" [arg1, arg2])  = [replaceWith $ Fun t (extend "clearBit" t) [arg1, arg2]]
    c99 (Fun t "complementBit" [arg1, arg2]) = [replaceWith $ Fun t (extend "complementBit" t) [arg1, arg2]]
    c99 (Fun t "testBit" [arg1, arg2])   = [replaceWith $ Fun t (extend "testBit" $ typeof arg1) [arg1, arg2]]
    c99 (Fun t "shiftL" [arg1, arg2])    = [replaceWith $ Binop t "<<" [arg1, arg2]]
    c99 (Fun t "shiftR" [arg1, arg2])    = [replaceWith $ Binop t ">>" [arg1, arg2]]
    c99 (Fun t "rotateL" [arg1, arg2])   = [replaceWith $ Fun t (extend "rotateL" t) [arg1, arg2]]
    c99 (Fun t "rotateR" [arg1, arg2])   = [replaceWith $ Fun t (extend "rotateR" t) [arg1, arg2]]
    c99 (Fun t "reverseBits" [arg])  = [replaceWith $ Fun t (extend "reverseBits" t) [arg]]
    c99 (Fun t "bitScan" [arg])  = [replaceWith $ Fun t (extend "bitScan" $ typeof arg) [arg]]
    c99 (Fun t "bitCount" [arg]) = [replaceWith $ Fun t (extend "bitCount" $ typeof arg) [arg]]
    c99 (Fun _ "bitSize" [intWidth . typeof -> Just n])  = [replaceWith $ LitI U32 n]
    c99 (Fun _ "isSigned" [intSigned . typeof -> Just b])    = [replaceWith $ litB b]
    c99 (Fun t "complex" [arg1, arg2])   = [replaceWith $ Fun t (extend "complex" $ typeof arg1) [arg1,arg2]]
    c99 (Fun t "creal" [arg])    = [replaceWith $ Fun t "crealf" [arg]]
    c99 (Fun t "cimag" [arg])    = [replaceWith $ Fun t "cimagf" [arg]]
    c99 (Fun t "conjugate" [arg])    = [replaceWith $ Fun t "conjf" [arg]]
    c99 (Fun t "magnitude" [arg])    = [replaceWith $ Fun t "cabsf" [arg]]
    c99 (Fun t "phase" [arg])    = [replaceWith $ Fun t "cargf" [arg]]
    c99 (Fun t "mkPolar" [arg1, arg2])   = [replaceWith $ Fun t (extend "mkPolar" $ typeof arg1) [arg1, arg2]]
    c99 (Fun t "cis" [arg])  = [replaceWith $ Fun t (extend "cis" $ typeof arg) [arg]]
    c99 (Fun t "f2i" [arg])  = [replaceWith $ Cast t $ Fun Floating "truncf" [arg]]
    c99 (Fun (Complex t) "i2n" [arg])    = [replaceWith $ Fun (Complex t) (extend "complex" t) [Cast t arg, LitF 0]]
    c99 (Fun t "i2n" [arg])  = [replaceWith $ Cast t arg]
    c99 (Fun t "b2i" [arg])  = [replaceWith $ Cast t arg]
    c99 (Fun t "round" [arg])    = [replaceWith $ Cast t $ Fun Floating "roundf" [arg]]
    c99 (Fun t "ceiling" [arg])  = [replaceWith $ Cast t $ Fun Floating "ceilf" [arg]]
    c99 (Fun t "floor" [arg])    = [replaceWith $ Cast t $ Fun Floating "floorf" [arg]]
    c99 _ = []

tic64xRules :: [Rule]
tic64xRules = [rule tic64x]
  where
    tic64x (Fun t "(==)" [arg1@(typeof -> Complex _), arg2])    = [replaceWith $ Fun t (extend "equal" $ typeof arg1) [arg1, arg2]]
    tic64x (Fun t "(/=)" [arg1@(typeof -> Complex _), arg2])    = [replaceWith $ Fun t "!" [Fun t (extend "equal" $ typeof arg1) [arg1, arg2]]]
    tic64x (Fun t "abs" [arg@(typeof -> Floating)]) = [replaceWith $ Fun t "_fabs" [arg]]
    tic64x (Fun t "abs" [arg@(typeof -> I32)])  = [replaceWith $ Fun t "_abs" [arg]]
    tic64x (Fun t "(+)" [arg1@(typeof -> Complex _), arg2]) = [replaceWith $ Fun t (extend "add" $ typeof arg1) [arg1, arg2]]
    tic64x (Fun t "(-)" [arg1@(typeof -> Complex _), arg2]) = [replaceWith $ Fun t (extend "sub" $ typeof arg1) [arg1, arg2]]
    tic64x (Fun t "(*)" [arg1@(typeof -> Complex _), arg2]) = [replaceWith $ Fun t (extend "mult" $ typeof arg1) [arg1, arg2]]
    tic64x (Fun t "(/)" [arg1@(typeof -> Complex _), arg2]) = [replaceWith $ Fun t (extend "div" $ typeof arg1) [arg1, arg2]]
    tic64x (Fun t "exp" [arg1@(typeof -> Complex _), arg2]) = [replaceWith $ Fun t (extend "exp" $ typeof arg1) [arg1, arg2]]
    tic64x (Fun t "sqrt" [arg1@(typeof -> Complex _), arg2])    = [replaceWith $ Fun t (extend "sqrt" $ typeof arg1) [arg1, arg2]]
    tic64x (Fun t "log" [arg1@(typeof -> Complex _), arg2]) = [replaceWith $ Fun t (extend "log" $ typeof arg1) [arg1, arg2]]
    tic64x (Fun t "(**)" [arg1@(typeof -> Complex _), arg2])    = [replaceWith $ Fun t (extend "cpow" $ typeof arg1) [arg1, arg2]]
    tic64x (Fun t "logBase" [arg1@(typeof -> Complex _), arg2]) = [replaceWith $ Fun t (extend "logBase" $ typeof arg1) [arg1, arg2]]
    tic64x (Fun t fn [arg@(typeof -> Complex _)])
        | fn `elem` ["sin","tan","cos","asin","atan","acos","sinh","tanh","cosh","asinh","atanh","acosh","creal","cimag","conjugate","magnitude","phase"]
            = [replaceWith $ Fun t (extend fn $ typeof arg) [arg]]
    tic64x (Fun t "rotateL" [arg1@(typeof -> U32), arg2])   = [replaceWith $ Fun t "_rotl" [arg1, arg2]]
    tic64x (Fun t "reverseBits" [arg@(typeof -> U32)])  = [replaceWith $ Fun t "_bitr" [arg]]
    tic64x (Fun t "bitCount" [arg@(typeof -> U32)])  = [replaceWith $ Fun t "_dotpu4" [Fun t "_bitc4" [arg], LitI U32 0x01010101]]
    tic64x (Fun t _ [arg@(typeof -> Complex _)]) = [replaceWith $ Fun t (extend "creal" $ typeof arg) [arg]]
    tic64x _ = []

traceRules :: [Rule]
traceRules = [rule trace]
  where
    trace (Fun t "trace" [lab, val]) = [WithId acts]
      where
       acts i = [replaceWith trcVar, propagate decl, propagate trc, propagate frame]
         where
            trcVar = Var t trcVarName
            trcVarName = "trc" ++ show i
            defTrcVar = Def (Variable t trcVarName)
            decl (Bl defs prg) = [replaceWith $ Bl (defs ++ [defTrcVar]) prg]
            trc :: Prog -> [Action (Repr Prog)]
            trc instr = [replaceWith $ Seq [trcVar := val,trcCall,instr]]
            trcCall = Call (extend' "trace" t) [In trcVar, In lab]
            frame (ProcDf pname knd ins outs prg) = [replaceWith $ ProcDf pname knd ins outs prg']
              where
                prg' = case prg of
                    Seq (Call "traceStart" [] : _) -> prg
                    Block _ (Seq (Call "traceStart" [] : _)) -> prg
                    _ -> Seq [Call "traceStart" [], prg, Call "traceEnd" []]
    trace _ = []

extend :: String -> Type -> String
extend s t = s ++ "_fun_" ++ show t

extend' :: String -> Type -> String
extend' s t = s ++ "_" ++ show t

log2 :: Integer -> Maybe Integer
log2 n
    | n == 2 Prelude.^ l    = Just l
    | otherwise             = Nothing
  where
    l = toInteger $ length $ takeWhile (<=n) $ map (2 Prelude.^) [1..]

first, second :: String
first  = "member1"
second = "member2"
