module Feldspar.Compiler.Backend.C.RuntimeLibrary where

import Data.Maybe (fromJust)
import Text.PrettyPrint

import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Backend.C.CodeGeneration (cgen, penv0)

machineLibrary :: Options -> [Entity ()]
machineLibrary opts =
  map (\t -> abs_fun opts (MachineVector 1 (NumType Signed t))) sizes ++
  map (\t -> pow_fun opts (MachineVector 1 (NumType Unsigned t))) sizes ++
  map (\t -> pow_fun opts (MachineVector 1 (NumType Signed t))) sizes ++
  map (\t -> signum_fun_u opts (MachineVector 1 (NumType Unsigned t))) sizes ++
  map (\t -> signum_fun_s opts (MachineVector 1 (NumType Signed t))) sizes ++
  map (\t -> signum_fun_f opts (MachineVector 1 t)) floats


sizes :: [Size]
sizes = [S8, S16, S32, S64]

floats :: [ScalarType]
floats = [FloatType, DoubleType]

{-
int8_t abs_fun_int8_t( int8_t a ) {
    // From Bit Twiddling Hacks:
    //    "Compute the integer absolute value (abs) without branching"
    int8_t mask = a >> 7;
    return (a + mask) ^ mask;
}
-}

abs_fun :: Options -> Type -> Entity ()
abs_fun opts typ = Proc name [inVar] (Right outVar) (Just body)
 where name   = "abs_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar  = Variable typ "a"
       inVar' = varToExpr inVar
       outVar = Variable typ "out"
       body   = Block lvars prg
       mVar   = Variable typ "mask"
       mVar'  = varToExpr mVar
       prg    = call "return" [ValueParameter $ binop typ "^" (binop typ "+" inVar' mVar') mVar']
       lvars  = [Declaration mVar (Just ival)]
       ival   = binop typ ">>" inVar' arg2
       arg2   = ConstExpr (IntConst (sizeToNum typ - 1) typ')
       typ'   = case typ of
                  MachineVector _ t -> t

{-
uint8_t pow_fun_uint8_t( uint8_t a, uint8_t b ) {
    uint8_t r = 1;
    int i;
    for(i = 0; i < b; ++i)
        r *= a;
    return r;
}
int8_t pow_fun_int8_t( int8_t a, int8_t b ) {
    int8_t r = 1;
    int i;
    if (b < 0) {
        fprintf(stderr, "Negative exponent in function pow_fun_(): %d `pow` %d", a, b);
        exit(1);
    }
    for(i = 0; i < b; ++i)
        r *= a;
    return r;
}
-}

pow_fun :: Options -> Type -> Entity ()
pow_fun opts typ = Proc name [inVar1, inVar2] (Right outVar) (Just body)
 where name   = "pow_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar1 = Variable typ "a"
       inVar1'= varToExpr inVar1
       inVar2 = Variable typ "b"
       inVar2'= varToExpr inVar2
       outVar = Variable typ "r"
       outVar'= varToExpr outVar
       lvars  = [Declaration outVar $ Just (litI typ 1)]
       body   = Block lvars prg
       prg    = Sequence [ guard
                         , for False "i" inVar2' (litI32 1) (Block [] body')
                         , call "return" [ValueParameter $ outVar']]
       body'  = Assign outVar' (binop typ "*" outVar' inVar1')
       guard
         | Just True <- intSigned typ
         = Sequence [ call "assert" [ ValueParameter $
                         binop (MachineVector 1 BoolType) "<" inVar2' (litI typ 0)]
                    , Comment False "Negative exponent in function pow_fun"]
         | otherwise = Empty

{-
int8_t signum_fun_int8_t( int8_t a ) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a != 0) | (a >> 7);
}
-}

signum_fun_s :: Options -> Type -> Entity ()
signum_fun_s opts typ = Proc name [inVar] (Right outVar) (Just body)
 where name   = "signum_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar  = Variable typ "a"
       inVar' = varToExpr inVar
       outVar = Variable typ "out"
       body   = toBlock prg
       prg    = call "return" [ValueParameter $
                   binop typ "|" (binop typ "!=" inVar' (litI typ 0)) ival]
       ival   = binop typ ">>" inVar' arg2
       arg2   = ConstExpr (IntConst (sizeToNum typ - 1) typ')
       typ'   = case typ of
                  MachineVector _ t -> t

{-
uint8_t signum_fun_uint8_t( uint8_t a ) {
    return (a > 0);
}
-}

signum_fun_u :: Options -> Type -> Entity ()
signum_fun_u opts typ = Proc name [inVar] (Right outVar) (Just body)
 where name   = "signum_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar  = Variable typ "a"
       inVar' = varToExpr inVar
       outVar = Variable typ "out"
       body   = toBlock prg
       prg    = call "return" [ValueParameter $
                   binop typ ">" inVar' (litI typ 0)]

{-
float signum_fun_float( float a ) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a > 0) - (a < 0);
}
-}

signum_fun_f :: Options -> Type -> Entity ()
signum_fun_f opts typ = Proc name [inVar] (Right outVar) (Just body)
 where name   = "signum_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar  = Variable typ "a"
       inVar' = varToExpr inVar
       outVar = Variable typ "out"
       body   = toBlock prg
       prg    = call "return" [ValueParameter $
                   binop typ "-" gzero szero]
       zero   = litI typ 0
       gzero  = binop typ ">" inVar' zero
       szero  = binop typ "<" inVar' zero

-- TODO: Add logBase() to complete the first section of feldspar_c99.c

sizeToNum :: Type -> Integer
sizeToNum = fromJust . intWidth

stderr :: Expression ()
stderr = varToExpr $ Variable VoidType "stderr"
