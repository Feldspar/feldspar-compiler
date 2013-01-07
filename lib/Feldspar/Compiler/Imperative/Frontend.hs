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

import Feldspar.Compiler.Imperative.Representation hiding (Alias, UserType, Cast, In, Out, Block, Pointer, Comment, NativeArray, NativeElem)
import qualified Feldspar.Compiler.Imperative.Representation as AIR

import Feldspar.Range
import Feldspar.Core.Types (Length)

-- * Frontend data types

data Mod = Mod [Ent]
  deriving (Show)

data Ent
    = StructD String [(String, Type)]
    | ProcDf String Kind [Variable ()] [Variable ()] Prog
    | ProcDcl String Kind [Variable ()] [Variable ()]
    deriving (Eq,Show)

data Expr
    = Var Type String
    | Ptr Type String
    | Lit EConst
    | Expr :!: Expr
    | Expr :.: String
    | NativeElem Expr Expr
    | Binop Type String [Expr]
    | Fun Type String [Expr]
    | Cast Type Expr
    | SizeofE Expr
    | SizeofT Type
    deriving (Eq,Show)

data EConst
    = EBool Bool
    | EFloat Double
    | EInt Type Integer
    | EComplex Expr Expr -- Asymmetry, should be some kind of number.
                         -- Necessary for Literal.hs.
    deriving (Eq,Show)

data Prog
    = Skip
    | Comment Bool String
    | Expr := Expr
    | Call String Kind [Param]
    | Seq [Prog]
    | If Expr Prog Prog
    | While Prog Expr Prog
    | For String Expr Int Prog
    | Block [Def] Prog
    deriving (Eq,Show)

instance Monoid Prog
  where
    mempty                    = Skip
    mappend Skip     p        = p
    mappend p        Skip     = p
    mappend (Seq pa) (Seq pb) = Seq (mappend pa pb)
    mappend pa pb             = Seq [mappend pa pb]

data Param
    = In Expr
    | Out Expr
    | TypAuto Type
    | TypScalar Type
    | Fn String Kind
    | FnAddr String Kind
    deriving (Eq,Show)

data Block = Bl [Def] Prog
    deriving (Eq,Show)

instance Monoid Block
  where
    mempty                        = Bl [] Skip
    mappend (Bl da pa) (Bl db pb) = Bl (mappend da db) (mappend pa pb)

data Def
    = Init (Variable ()) Expr
    | Def (Variable ())
    deriving (Eq,Show)

class Named a where
    getName :: a -> String
instance Named Def where
    getName (Init v _) = getName v
    getName (Def v)    = getName v
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

instance Interface Expr where
    type Repr Expr = Expression ()
    toInterface (VarExpr (AIR.Variable AIR.Value   t name)) = Var t name
    toInterface (VarExpr (AIR.Variable AIR.Pointer t name)) = Ptr t name
    toInterface (ArrayElem arr idx) = toInterface arr :!: toInterface idx
    toInterface (AIR.NativeElem arr idx) = NativeElem (toInterface arr) (toInterface idx)
    toInterface (StructField str field) = toInterface str :.: field
    toInterface (ConstExpr (BoolConst True)) = litB True
    toInterface (ConstExpr (BoolConst False)) = litB False
    toInterface (ConstExpr (IntConst x t)) = litI t x
    toInterface (ConstExpr (FloatConst x)) = litF x
    toInterface (ConstExpr (ComplexConst r i)) = litC (toInterface $ ConstExpr r) (toInterface $ ConstExpr i)
    toInterface (FunctionCall (Function name t Prefix) ps) = Fun t name $ map toInterface ps
    toInterface (FunctionCall (Function name t Infix) ps) = Binop t name $ map toInterface ps
    toInterface (AIR.Cast t e) = Cast t (toInterface e)
    toInterface (SizeOf (Left t)) = SizeofT t
    toInterface (SizeOf (Right e)) = SizeofE (toInterface e)
    fromInterface (Var t name) = VarExpr (AIR.Variable AIR.Value   t name)
    fromInterface (Ptr t name) = VarExpr (AIR.Variable AIR.Pointer t name)
    fromInterface (Lit (EBool b)) = ConstExpr (BoolConst b)
    fromInterface (Lit (EInt t x)) = ConstExpr (IntConst x t)
    fromInterface (Lit (EFloat x)) = ConstExpr (FloatConst x)
    fromInterface (Lit (EComplex
                        (fromInterface -> ConstExpr r)
                        (fromInterface -> ConstExpr i))) =
                        ConstExpr (ComplexConst r i)
    fromInterface (Lit (EComplex _ _)) = error "Internal compiler error for complex literal"
    fromInterface (Binop t name es) = FunctionCall (Function name t Infix) (map fromInterface es)
    fromInterface (Fun t name es) = FunctionCall (Function name t Prefix) (map fromInterface es)
    fromInterface (Cast t e) = AIR.Cast t (fromInterface e)
    fromInterface (SizeofE e) = SizeOf (Right $ fromInterface e)
    fromInterface (SizeofT t) = SizeOf (Left t)
    fromInterface (arr :!: idx) = ArrayElem (fromInterface arr) (fromInterface idx)
    fromInterface (NativeElem arr idx) = AIR.NativeElem (fromInterface arr) (fromInterface idx)
    fromInterface (str :.: field) = StructField (fromInterface str) field

instance Interface Prog where
    type Repr Prog = AIR.Program ()
    toInterface (Empty) = Skip
    toInterface (AIR.Comment b s) = Comment b s
    toInterface Assign{..} = toInterface lhs := toInterface rhs
    toInterface (ProcedureCall s k ps) = Call s k (map toInterface ps)
    toInterface (Sequence ps) = Seq (map toInterface ps)
    toInterface (Branch e b1 b2) = If (toInterface e) (toProg b1) (toProg b2)
    toInterface (Switch e alts) = error "TODO: toInterface Switch"
    toInterface (SeqLoop e pe b) = While (toProg pe) (toInterface e) (toProg b)
    toInterface (ParLoop v e i b) = For (varName v) (toInterface e) i (toProg b)
    toInterface (BlockProgram b) = Block (map toInterface $ locals b) (toInterface $ blockBody b)
    fromInterface (Skip) = Empty
    fromInterface (Comment b s) = AIR.Comment b s
    fromInterface (lhs := rhs) = Assign (fromInterface lhs) (fromInterface rhs)
    fromInterface (Call s k ps) = ProcedureCall s k (map fromInterface ps)
    fromInterface (Seq ps) = Sequence (map fromInterface ps)
    fromInterface (If e p1 p2) = Branch (fromInterface e) (toBlock p1) (toBlock p2)
--    fromInterface (Switch scrut alts) = Switch (fromInterface scrut) (map toBlock alts) () () -- TODO: Add Switch in Prog.
    fromInterface (While pe e p) = SeqLoop (fromInterface e) (toBlock pe) (toBlock p)
    fromInterface (For s e i p) = ParLoop
        (AIR.Variable Value (NumType Unsigned S32) s) (fromInterface e) i (toBlock p)
    fromInterface (Block ds p) = BlockProgram (AIR.Block (map fromInterface ds) (fromInterface p))

instance Interface Param where
    type Repr Param = ActualParameter ()
    toInterface (AIR.In e) = In (toInterface e)
    toInterface (AIR.Out e) = Out (toInterface e)
    toInterface (AIR.TypeParameter e AIR.Auto) = TypAuto e
    toInterface (AIR.TypeParameter e AIR.Scalar) = TypScalar e
    toInterface (AIR.FunParameter n k False) = Fn n k
    toInterface (AIR.FunParameter n k True) = FnAddr n k
    fromInterface (In e) = AIR.In (fromInterface e)
    fromInterface (Out e) = AIR.Out (fromInterface e)
    fromInterface (TypAuto e) = AIR.TypeParameter e Auto
    fromInterface (TypScalar e) = AIR.TypeParameter e Scalar
    fromInterface (Fn n k) = AIR.FunParameter n k False
    fromInterface (FnAddr n k) = AIR.FunParameter n k True

instance Interface Def where
    type Repr Def = Declaration ()
    toInterface (Declaration v (Just e)) = Init v (toInterface e)
    toInterface (Declaration v Nothing) = Def v
    fromInterface (Init v e) = Declaration v (Just $ fromInterface e)
    fromInterface (Def v) = Declaration v  Nothing

instance Interface Block where
    type Repr Block = AIR.Block ()
    toInterface (AIR.Block ds p) = Bl (map toInterface ds) (toInterface p)
    fromInterface (Bl ds p) = AIR.Block (map fromInterface ds) (fromInterface p)

instance Interface (Variable t) where
    type Repr (Variable t) = AIR.Variable t
    toInterface = id
    fromInterface = id

toBlock :: Prog -> AIR.Block ()
toBlock (Block ds p) = AIR.Block (map fromInterface ds) (fromInterface p)
toBlock p = AIR.Block [] (fromInterface p)

toProg :: AIR.Block () -> Prog
toProg (AIR.Block [] p) = toInterface p
toProg (AIR.Block ds p) = Block (map toInterface ds) (toInterface p)

setLength :: Expr -> Expr -> Prog
setLength arr len = Call "setLength" KNormal [Out arr, In len]

-- | Copies expressions into a destination. If the destination is
-- a non-scalar the arguments are appended to the destination.
copyProg :: Expr -> [Expr] -> Prog
copyProg _ [] = error "copyProg: missing source parameter."
copyProg outExp inExp
    | outExp == (head inExp)
      && null (tail inExp) = Skip
    | otherwise            = Call "copy" KNormal (Out outExp:map In inExp)

copyProgPos :: Expr -> Expr -> Expr -> Prog
copyProgPos outExp shift inExp = Call "copyArrayPos" KNormal [Out outExp, In shift, In inExp]

copyProgLen :: Expr -> Expr -> Expr -> Prog
copyProgLen outExp inExp len = Call "copyArrayLen" KNormal [Out outExp, In inExp, In len]

initArray :: Expr -> Expr -> Prog
initArray arr len = Call "initArray" KNormal [Out arr, In s, In len]
  where
    s
        | isArray t = Binop (NumType Unsigned S32) "-" [litI (NumType Unsigned S32) 0,SizeofT t]
        | otherwise = SizeofT t
    t = case typeof arr of
        ArrayType _ e -> e
        _       -> error $ "Feldspar.Compiler.Imperative.Frontend.initArray: invalid type of array " ++ show arr ++ "::" ++ show (typeof arr)

assignProg :: Expr -> Expr -> Prog
assignProg inExp outExp = copyProg inExp [outExp]

freeArray :: Variable t -> Prog
freeArray arr = Call "freeArray" KNormal [Out $ varToExpr arr]

freeArrays :: [Def] -> [Prog]
freeArrays defs = map freeArray arrays
  where
    arrays = filter (isArray . typeof) $ map dVar defs

arrayLength :: Expr -> Expr
arrayLength arr
  | Just r <- chaseArray arr = litI (NumType Unsigned S32) $ fromIntegral (upperBound r)
  | otherwise = Fun (NumType Unsigned S32) "getLength" [arr]

chaseArray :: Expr -> Maybe (Range Length)
chaseArray e = go e []  -- TODO: Extend to handle x.member1.member2
  where go :: Expr -> [String] -> Maybe (Range Length)
        go (Var (ArrayType r _) _) [] | isSingleton r = Just r
        go (Ptr (ArrayType r _) _) [] | isSingleton r = Just r
        go (e :.: s) ss = go e (s:ss)
        go (Var (StructType fields) _) (s:_)
          | Just (ArrayType r _) <- lookup s fields
          , isSingleton r = Just r
        go (Ptr (StructType fields) _) (s:_)
          | Just (ArrayType r _) <- lookup s fields
          , isSingleton r = Just r
        go _ _ = Nothing

iVarInit :: Expr -> Prog
iVarInit var = Call "ivar_init" KIVar [Out var]

iVarGet :: Expr -> Expr -> Prog
iVarGet loc ivar 
    | isArray typ   = Call "ivar_get_array" KIVar [Out loc, In ivar]
    | otherwise     = Call "ivar_get" KIVar [TypScalar typ, Out loc, In ivar]
      where
        typ = typeof loc

iVarPut :: Expr -> Expr -> Prog
iVarPut ivar msg
    | isArray typ   = Call "ivar_put_array" KIVar [In ivar, Out msg]
    | otherwise     = Call "ivar_put" KIVar [TypAuto typ, In ivar, Out msg]
      where
        typ = typeof msg

iVarDestroy :: Variable t -> Prog
iVarDestroy v = Call "ivar_destroy" KIVar [Out $ varToExpr v]

freeIVars :: [Def] -> [Prog]
freeIVars defs = map iVarDestroy ivars
  where
    ivars = filter (isIVar . typeof) $ map dVar defs

spawn :: String -> [Variable t] -> Prog
spawn taskName vs = Call spawnName KTask allParams
  where
    spawnName = "spawn" ++ show (length vs)
    taskParam = FnAddr taskName KTask
    typeParams = map (TypAuto . vType) vs
    varParams = map (\v -> In $ Var (vType v) (vName v)) vs
    allParams = taskParam : concat (zipWith (\a b -> [a,b]) typeParams varParams)

run :: String -> [Variable t] -> Prog
run taskName vs = Call runName KTask allParams
  where
    runName = "run" ++ show (length vs)
    typeParams = map (TypAuto . vType) vs
    taskParam = Fn taskName KTask
    allParams = taskParam : typeParams

instance HasType Expr
  where
    type TypeOf Expr = Type
    typeof = typeof . fromInterface

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

litF :: Double -> Expr
litF n = Lit (EFloat n)

litB :: Bool -> Expr
litB True = Lit (EBool True)
litB False = Lit (EBool False)

litC :: Expr -> Expr -> Expr
litC r i = Lit (EComplex r i)

litI :: Type -> Integer -> Expr
litI t n = Lit (EInt t n)

litI32 :: Integer -> Expr
litI32 n = litI (NumType Unsigned S32) n

isArray :: Type -> Bool
isArray ArrayType{} = True
isArray _ = False

isIVar :: Type -> Bool
isIVar AIR.IVarType{} = True
isIVar _              = False

vType :: Variable t -> Type
vType Variable{..} = varType

dVar :: Def -> Variable ()
dVar (Def v)    = v
dVar (Init v _) = v

vName :: Variable t -> String
vName Variable{..} = varName

lName :: Expr -> String
lName (Var _ s) = s
lName (e :!: _) = lName e
lName (e :.: _) = lName e
lName e         = error $ "Feldspar.Compiler.Imperative.Frontend.lName: invalid location: " ++ show e

varToExpr :: Variable t -> Expr
varToExpr (Variable AIR.Value   t name) = Var t name
varToExpr (Variable AIR.Pointer t name) = Ptr t name
