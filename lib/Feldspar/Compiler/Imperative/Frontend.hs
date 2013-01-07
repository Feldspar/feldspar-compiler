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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Feldspar.Compiler.Imperative.Frontend where

import Data.List (intercalate)
import Data.Monoid (Monoid(..))
import Control.Arrow (second)

import Feldspar.Compiler.Imperative.Representation hiding (Alias, UserType)
import qualified Feldspar.Compiler.Imperative.Representation as AIR

import Feldspar.Range
import Feldspar.Core.Types (Length)

-- * Frontend data types

data Mod = Mod [Ent]
  deriving (Show)

data Ent
    = StructD String [(String, Type)]
    | ProcDf String Kind [Variable ()] [Variable ()] (Program ())
    | ProcDcl String Kind [Variable ()] [Variable ()]
    deriving (Eq,Show)

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

class Named a where
    getName :: a -> String
instance Named (Declaration t) where
    getName (Declaration v _) = getName v
instance Named (Variable t) where
    getName = varName

-- * Conversion between representation and frontend

class Interface t where
    type Repr t
    toInterface :: Repr t -> t
    fromInterface :: t -> Repr t

instance Interface Mod where
    type Repr Mod = AIR.Module ()
    toInterface (Module es) = Mod $ map toInterface es
    fromInterface (Mod es) = AIR.Module (map fromInterface es)

instance Interface Ent where
    type Repr Ent = AIR.Entity ()
    toInterface (AIR.StructDef name members) =
        StructD name (map (\(StructMember mname mtyp)->(mname, mtyp)) members)
    toInterface (AIR.ProcDef name knd inparams outparams body) =
        ProcDf name knd inparams outparams (toProg body)
    toInterface (AIR.ProcDecl name knd inparams outparams) =
        ProcDcl name knd inparams outparams
    toInterface AIR.TypeDef{} = error "TypeDef not handled"
    fromInterface (StructD name members) =
        AIR.StructDef name (map (\(mname,mtyp)->(StructMember mname mtyp)) members)
    fromInterface (ProcDf name knd inparams outparams body) =
        AIR.ProcDef name knd inparams outparams (toBlock body)
    fromInterface (ProcDcl name knd inparams outparams) =
        AIR.ProcDecl name knd inparams outparams

instance Interface (Expression t) where
    type Repr (Expression t) = AIR.Expression t
    toInterface = id
    fromInterface = id

instance Interface (Program t) where
    type Repr (Program t) = AIR.Program t
    toInterface = id
    fromInterface = id

instance Interface (ActualParameter t) where
    type Repr (ActualParameter t) = ActualParameter t
    toInterface = id
    fromInterface = id

instance Interface (Declaration t) where
    type Repr (Declaration t) = AIR.Declaration t
    toInterface = id
    fromInterface = id

instance Interface (Block t) where
    type Repr (Block t) = AIR.Block t
    toInterface = id
    fromInterface = id

instance Interface (Variable t) where
    type Repr (Variable t) = AIR.Variable t
    toInterface = id
    fromInterface = id

toBlock :: Program () -> AIR.Block ()
toBlock (BlockProgram b) = b

toProg :: AIR.Block () -> Program ()
toProg (AIR.Block [] p) = toInterface p
toProg e = BlockProgram e

setLength :: Expression () -> Expression () -> Program ()
setLength arr len = call "setLength" KNormal [Out arr, In len]

-- | Copies expressions into a destination. If the destination is
-- a non-scalar the arguments are appended to the destination.
copyProg :: Expression ()-> [Expression ()] -> Program ()
copyProg _ [] = error "copyProg: missing source parameter."
copyProg outExp inExp
    | outExp == (head inExp)
      && null (tail inExp) = Empty
    | otherwise            = call "copy" KNormal (Out outExp:map In inExp)

copyProgPos :: Expression ()-> Expression () -> Expression () -> Program ()
copyProgPos outExp shift inExp = call "copyArrayPos" KNormal [Out outExp, In shift, In inExp]

copyProgLen :: Expression () -> Expression () -> Expression () -> Program ()
copyProgLen outExp inExp len = call "copyArrayLen" KNormal [Out outExp, In inExp, In len]

initArray :: Expression () -> Expression () -> Program ()
initArray arr len = call "initArray" KNormal [Out arr, In s, In len]
  where
    s
        | isArray t = FunctionCall (Function "-" (NumType Unsigned S32) Infix) [litI (NumType Unsigned S32) 0,SizeOf (Left t)]
        | otherwise = SizeOf (Left t)
    t = case typeof arr of
        ArrayType _ e -> e
        _       -> error $ "Feldspar.Compiler.Imperative.Frontend.initArray: invalid type of array " ++ show arr ++ "::" ++ show (typeof arr)

assignProg :: Expression () -> Expression () -> Program ()
assignProg inExp outExp = copyProg inExp [outExp]

freeArray :: Variable () -> Program ()
freeArray arr = call "freeArray" KNormal [Out $ varToExpr arr]

freeArrays :: [Declaration ()] -> [Program ()]
freeArrays defs = map freeArray arrays
  where
    arrays = filter (isArray . typeof) $ map dVar defs

arrayLength :: Expression () -> Expression ()
arrayLength arr
  | Just r <- chaseArray arr = litI (NumType Unsigned S32) $ fromIntegral (upperBound r)
  | otherwise = FunctionCall (Function "getLength" (NumType Unsigned S32) Prefix) [arr]

chaseArray :: Expression t-> Maybe (Range Length)
chaseArray e = go e []  -- TODO: Extend to handle x.member1.member2
  where go :: Expression t-> [String] -> Maybe (Range Length)
        go (VarExpr (Variable _ (ArrayType r _) _)) [] | isSingleton r = Just r
        go (StructField e s) ss = go e (s:ss)
        go (VarExpr (Variable _ (StructType fields) _)) (s:_)
          | Just (ArrayType r _) <- lookup s fields
          , isSingleton r = Just r
        go _ _ = Nothing

iVarInit :: Expression () -> Program ()
iVarInit var = call "ivar_init" KIVar [Out var]

iVarGet :: Expression () -> Expression () -> Program ()
iVarGet loc ivar 
    | isArray typ   = call "ivar_get_array" KIVar [Out loc, In ivar]
    | otherwise     = call "ivar_get" KIVar [TypeParameter typ Scalar, Out loc, In ivar]
      where
        typ = typeof loc

iVarPut :: Expression () -> Expression () -> Program ()
iVarPut ivar msg
    | isArray typ   = call "ivar_put_array" KIVar [In ivar, Out msg]
    | otherwise     = call "ivar_put" KIVar [TypeParameter typ Auto, In ivar, Out msg]
      where
        typ = typeof msg

iVarDestroy :: Variable () -> Program ()
iVarDestroy v = call "ivar_destroy" KIVar [Out $ varToExpr v]

freeIVars :: [Declaration ()] -> [Program ()]
freeIVars defs = map iVarDestroy ivars
  where
    ivars = filter (isIVar . typeof) $ map dVar defs

spawn :: String -> [Variable ()] -> Program ()
spawn taskName vs = call spawnName KTask allParams
  where
    spawnName = "spawn" ++ show (length vs)
    taskParam = FunParameter taskName KTask True
    typeParams = map ((\t -> TypeParameter t Auto) . vType) vs
    varParams = map (\v -> In $ VarExpr (Variable Value (vType v) (vName v))) vs
    allParams = taskParam : concat (zipWith (\a b -> [a,b]) typeParams varParams)

run :: String -> [Variable ()] -> Program ()
run taskName vs = call runName KTask allParams
  where
    runName = "run" ++ show (length vs)
    typeParams = map ((\t -> TypeParameter t Auto) . vType) vs
    taskParam = FunParameter taskName KTask False
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

litF :: Double -> Expression t
litF n = ConstExpr (FloatConst n)

litB :: Bool -> Expression ()
litB b = ConstExpr (BoolConst b)

litC :: Constant () -> Constant () -> Expression ()
litC r i = ConstExpr (ComplexConst r i)

litI :: Type -> Integer -> Expression ()
litI t n = ConstExpr (IntConst n t)

litI32 :: Integer -> Expression ()
litI32 n = litI (NumType Unsigned S32) n

isArray :: Type -> Bool
isArray ArrayType{} = True
isArray _ = False

isIVar :: Type -> Bool
isIVar AIR.IVarType{} = True
isIVar _              = False

vType :: Variable () -> Type
vType Variable{..} = varType

dVar :: Declaration () -> Variable ()
dVar (Declaration v _)    = v

vName :: Variable t -> String
vName Variable{..} = varName

lName :: Expression t -> String
lName (VarExpr v@Variable{}) = vName v
lName (ArrayElem e _)        = lName e
lName (StructField e _)      = lName e
lName e                      = error $ "Feldspar.Compiler.Imperative.Frontend.lName: invalid location: " ++ show e

varToExpr :: Variable t -> Expression t
varToExpr v = VarExpr v

binop :: Type -> String -> Expression () -> Expression () -> Expression ()
binop t n e1 e2 = fun' Infix t n [e1, e2]

fun :: Type -> String -> [Expression ()] -> Expression ()
fun = fun' Prefix

fun' :: FunctionMode -> Type -> String -> [Expression ()] -> Expression ()
fun' m t n es = FunctionCall (Function n t m) es

call :: String -> Kind -> [ActualParameter ()] -> Program ()
call n k ps = ProcedureCall n k ps

for :: String -> Expression () -> Int -> Block () -> Program ()
for s e i p = ParLoop (Variable Value (NumType Unsigned S32) s) e i p

while :: Program () -> Expression () -> Program () -> Program ()
while p e b = SeqLoop e (Block [] (Sequence [p])) (Block [] (Sequence [b]))
