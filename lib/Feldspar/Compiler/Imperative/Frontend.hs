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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Feldspar.Compiler.Imperative.Frontend where

import Feldspar.Compiler.Imperative.Representation

import Feldspar.Range
import Feldspar.Core.Types (Length)

toBlock :: Program () -> Block ()
toBlock (BlockProgram b) = b
toBlock p                = Block [] p

toProg :: Block () -> Program ()
toProg (Block [] p) = p
toProg e = BlockProgram e

-- | Copies expressions into a destination. If the destination is
-- a non-scalar the arguments are appended to the destination.
copyProg :: Maybe (Expression ())-> [Expression ()] -> Program ()
copyProg _ [] = error "copyProg: missing source parameter."
copyProg Nothing _ = Empty
copyProg (Just outExp) inExp
    | outExp == head inExp
      && null (tail inExp) = Empty
    | otherwise            = call "copy" (map ValueParameter (outExp:inExp))

mkInitialize :: String -> Maybe (Expression ()) -> Expression () -> Program ()
mkInitialize _    Nothing    _   = Empty
mkInitialize name (Just arr) len = Assign arr $ fun (typeof arr) name [arr, sz, len]
  where
    sz | isArray t' = binop (NumType Unsigned S32) "-" (litI32 0) t
       | otherwise  = t
    t = SizeOf t'
    t' = go $ typeof arr
    go (ArrayType _ e) = e
    go (Pointer typ)   = go typ
    go _               = error $ "Feldspar.Compiler.Imperative.Frontend." ++ name ++ ": invalid type of array " ++ show arr ++ "::" ++ show (typeof arr)

initArray :: Maybe (Expression ()) -> Expression () -> Program ()
initArray = mkInitialize "initArray"

setLength :: Maybe (Expression ()) -> Expression () -> Program ()
setLength = mkInitialize "setLength"

freeArray :: Variable () -> Program ()
freeArray arr = call "freeArray" [ValueParameter $ varToExpr arr]

freeArrays :: [Declaration ()] -> [Program ()]
freeArrays defs = map freeArray arrays
  where
    arrays = filter (isArray . typeof) $ map dVar defs

arrayLength :: Expression () -> Expression ()
arrayLength arr
  | Just r <- chaseArray arr = litI32 $ fromIntegral (upperBound r)
  | otherwise = FunctionCall (Function "getLength" (NumType Unsigned S32) Prefix) [arr]

chaseArray :: Expression t-> Maybe (Range Length)
chaseArray = go []  -- TODO: Extend to handle x.member1.member2
  where go :: [String] -> Expression t -> Maybe (Range Length)
        go []    (ConstExpr (ArrayConst l)) = Just (singletonRange $ fromIntegral $ length l)
        go []    (VarExpr (Variable (ArrayType r _) _)) | isSingleton r = Just r
        go []    (VarExpr (Variable (NativeArray (Just r) _) _)) = Just (singletonRange r)
        go ss    (StructField e s) = go (s:ss) e
        go ss    (AddrOf e) = go ss e
        go (s:_) (VarExpr (Variable (StructType _ fields) _))
          | Just (ArrayType r _) <- lookup s fields
          , isSingleton r = Just r
          | Just (NativeArray (Just r) _) <- lookup s fields
          = Just (singletonRange r)
        go _ _ = Nothing

iVarInit :: Expression () -> Program ()
iVarInit var = call "ivar_init" [ValueParameter var]

iVarGet :: Expression () -> Expression () -> Program ()
iVarGet loc ivar 
    | isArray typ   = call "ivar_get_array" [ValueParameter loc, ValueParameter ivar]
    | otherwise     = call "ivar_get" [TypeParameter typ, ValueParameter (AddrOf loc), ValueParameter ivar]
      where
        typ = typeof loc

iVarPut :: Expression () -> Expression () -> Program ()
iVarPut ivar msg
    | isArray typ   = call "ivar_put_array" [ValueParameter ivar, ValueParameter  msg]
    | otherwise     = call "ivar_put" [TypeParameter typ, ValueParameter ivar, ValueParameter (AddrOf msg)]
      where
        typ = typeof msg

iVarDestroy :: Variable () -> Program ()
iVarDestroy v = call "ivar_destroy" [ValueParameter $ AddrOf $ varToExpr v]

freeIVars :: [Declaration ()] -> [Program ()]
freeIVars defs = map iVarDestroy ivars
  where
    ivars = filter (isIVar . typeof) $ map dVar defs

spawn :: String -> [Variable ()] -> Program ()
spawn taskName vs = call spawnName allParams
  where
    spawnName = "spawn" ++ show (length vs)
    taskParam = FunParameter taskName
    typeParams = map (TypeParameter . typeof) vs
    varParams = map (ValueParameter . mkv) vs
      where mkv v = let v' = VarExpr $ Variable (typeof v) (vName v)
                    in if isPointer $ typeof v then AddrOf v' else v'
    allParams = taskParam : concat (zipWith (\a b -> [a,b]) typeParams varParams)

run :: String -> [Variable ()] -> Program ()
run taskName vs = call runName allParams
  where
    runName = "run" ++ show (length vs)
    typeParams = map (TypeParameter . typeof) vs
    taskParam = FunParameter taskName
    allParams = taskParam : typeParams

intWidth :: Type -> Maybe Integer
intWidth (NumType _ S8)  = Just 8
intWidth (NumType _ S16) = Just 16
intWidth (NumType _ S32) = Just 32
intWidth (NumType _ S40) = Just 40
intWidth (NumType _ S64) = Just 64
intWidth _               = Nothing

intSigned :: Type -> Maybe Bool
intSigned (NumType Unsigned _) = Just False
intSigned (NumType Signed _)   = Just True
intSigned _                    = Nothing

litF :: Float -> Expression t
litF n = ConstExpr (FloatConst n)

litD :: Double -> Expression t
litD n = ConstExpr (DoubleConst n)

litB :: Bool -> Expression ()
litB b = ConstExpr (BoolConst b)

litC :: Constant () -> Constant () -> Expression ()
litC r i = ConstExpr (ComplexConst r i)

litI :: Type -> Integer -> Expression ()
litI t n = ConstExpr (IntConst n t)

litI32 :: Integer -> Expression ()
litI32 = litI (NumType Unsigned S32)

isArray :: Type -> Bool
isArray ArrayType{} = True
isArray (Pointer t) = isArray t
isArray _ = False

isNativeArray :: Type -> Bool
isNativeArray NativeArray{} = True
isNativeArray (Pointer t)   = isNativeArray t
isNativeArray _             = False

isIVar :: Type -> Bool
isIVar IVarType{} = True
isIVar _              = False

isPointer :: Type -> Bool
isPointer Pointer{} = True
isPointer _         = False

dVar :: Declaration () -> Variable ()
dVar (Declaration v _)    = v

vName :: Variable t -> String
vName Variable{..} = varName

lName :: Expression t -> String
lName (VarExpr v@Variable{}) = vName v
lName (ArrayElem e _)        = lName e
lName (StructField e _)      = lName e
lName (AddrOf e)             = lName e
lName e                      = error $ "Feldspar.Compiler.Imperative.Frontend.lName: invalid location: " ++ show e

varToExpr :: Variable t -> Expression t
varToExpr = VarExpr

binop :: Type -> String -> Expression () -> Expression () -> Expression ()
binop t n e1 e2 = fun' Infix t n [e1, e2]

fun :: Type -> String -> [Expression ()] -> Expression ()
fun = fun' Prefix

fun' :: FunctionMode -> Type -> String -> [Expression ()] -> Expression ()
fun' m t n = FunctionCall (Function n t m)

call :: String -> [ActualParameter ()] -> Program ()
call = ProcedureCall

for :: Bool -> String -> Expression () -> Expression () -> Block () -> Program ()
for _ _ _ _ (Block [] (Sequence [Empty])) = Empty
for p s e i b = ParLoop p (Variable (NumType Unsigned S32) s) e i b

while :: Block () -> Expression () -> Block () -> Program ()
while p e = SeqLoop e p

