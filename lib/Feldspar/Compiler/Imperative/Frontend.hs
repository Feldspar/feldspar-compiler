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

import Data.List
import Data.Monoid
import Control.Arrow (second)

import Feldspar.Compiler.Imperative.Representation hiding (Alias, Type, UserType, Cast, In, Out, Variable, Block, Pointer, Comment, Spawn, Run, NativeArray, NativeElem)
import qualified Feldspar.Compiler.Imperative.Representation as AIR

import Feldspar.Range
import Feldspar.Core.Types (Length)

-- * Frontend data types

data Mod = Mod [Ent]
  deriving (Show)

data Ent
    = StructD String [(String, Type)]
    | ProcDf String Kind [Var] [Var] Prog
    | ProcDcl String Kind [Var] [Var]
    deriving (Eq,Show)

data Type
    = Void
    | Boolean
    | Bit
    | Floating
    | I8 | I16 | I32 | I40 | I64
    | U8 | U16 | U32 | U40 | U64
    | Complex Type
    | UserType String
    | SizedArray (Range Length) Type
    | NativeArray (Maybe Length) Type
    | Struct [(String, Type)]
    | IVar Type
    | Alias Type String
  deriving Eq

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
    | Comment Bool {-BlockComment-} String
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
    mappend (Seq pa) (Seq pb) = Seq $ mappend pa pb
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
    = Init Var Expr
    | Def Var
    deriving (Eq,Show)

data Var
    = Variable Type String
    | Pointer Type String
    deriving (Eq,Show)

class Named a where
    getName :: a -> String
instance Named Def where
    getName (Init v _) = getName v
    getName (Def v)    = getName v
instance Named Var where
    getName (Variable _ n) = n
    getName (Pointer  _ n) = n

-- * Conversion between representation and frontend

class Interface t where
    type Repr t
    toInterface :: Repr t -> t
    fromInterface :: t -> Repr t

instance Interface Mod where
    type Repr Mod = AIR.Module ()
    toInterface (Module es ()) = Mod $ map toInterface es
    fromInterface (Mod es) = AIR.Module (map fromInterface es) ()

instance Interface Ent where
    type Repr Ent = AIR.Entity ()
    toInterface (AIR.StructDef name members () ()) =
        StructD name (map (\(StructMember mname mtyp ())->(mname,toInterface mtyp)) members)
    toInterface (AIR.ProcDef name knd inparams outparams body () ()) =
        ProcDf name knd (map toInterface inparams) (map toInterface outparams) (toProg body)
    toInterface (AIR.ProcDecl name knd inparams outparams () ()) =
        ProcDcl name knd (map toInterface inparams) (map toInterface outparams)
    toInterface AIR.TypeDef{} = error "TypeDef not handled"
    fromInterface (StructD name members) =
        AIR.StructDef name (map (\(mname,mtyp)->(StructMember mname (fromInterface mtyp) ())) members) () ()
    fromInterface (ProcDf name knd inparams outparams body) =
        AIR.ProcDef name knd (map fromInterface inparams) (map fromInterface outparams) (toBlock body) () ()
    fromInterface (ProcDcl name knd inparams outparams) =
        AIR.ProcDecl name knd (map fromInterface inparams) (map fromInterface outparams) () ()

instance Interface Type where
    type Repr Type = AIR.Type
    toInterface VoidType = Void
    toInterface (AIR.Alias t s) = Alias (toInterface t) s
    toInterface AIR.BoolType = Boolean
    toInterface BitType = Bit
    toInterface AIR.FloatType = Floating
    toInterface (NumType Signed S8) = I8
    toInterface (NumType Signed S16) = I16
    toInterface (NumType Signed S32) = I32
    toInterface (NumType Signed S40) = I40
    toInterface (NumType Signed S64) = I64
    toInterface (NumType Unsigned S8) = U8
    toInterface (NumType Unsigned S16) = U16
    toInterface (NumType Unsigned S32) = U32
    toInterface (NumType Unsigned S40) = U40
    toInterface (NumType Unsigned S64) = U64
    toInterface (AIR.ComplexType t) = Complex $ toInterface t
    toInterface (AIR.UserType s) = UserType s
    toInterface (AIR.ArrayType l t) = SizedArray l $ toInterface t
    toInterface (AIR.NativeArray l t) = NativeArray l $ toInterface t
    toInterface (AIR.StructType fields) = Struct $ map (second toInterface) fields
    toInterface (AIR.IVarType t) = IVar $ toInterface t
    fromInterface Void = VoidType
    fromInterface (Alias t s) = AIR.Alias (fromInterface t) s
    fromInterface Boolean = AIR.BoolType
    fromInterface Bit = BitType
    fromInterface Floating = AIR.FloatType
    fromInterface I8 = NumType Signed S8
    fromInterface I16 = NumType Signed S16
    fromInterface I32 = NumType Signed S32
    fromInterface I40 = NumType Signed S40
    fromInterface I64 = NumType Signed S64
    fromInterface U8 = NumType Unsigned S8
    fromInterface U16 = NumType Unsigned S16
    fromInterface U32 = NumType Unsigned S32
    fromInterface U40 = NumType Unsigned S40
    fromInterface U64 = NumType Unsigned S64
    fromInterface (Complex t) = AIR.ComplexType $ fromInterface t
    fromInterface (UserType s) = AIR.UserType s
    fromInterface (SizedArray l t) = AIR.ArrayType l $ fromInterface t
    fromInterface (NativeArray l t) = AIR.NativeArray l $ fromInterface t
    fromInterface (Struct fields) = AIR.StructType $ map (second fromInterface) fields
    fromInterface (IVar t) = AIR.IVarType $ fromInterface t

instance Interface Expr where
    type Repr Expr = Expression ()
    toInterface (VarExpr (AIR.Variable name t Value ()) ()) = Var (toInterface t) name
    toInterface (VarExpr (AIR.Variable name t AIR.Pointer ()) ()) = Ptr (toInterface t) name
    toInterface (ArrayElem arr idx () ()) = toInterface arr :!: toInterface idx
    toInterface (AIR.NativeElem arr idx () ()) = NativeElem (toInterface arr) (toInterface idx)
    toInterface (StructField str field () ()) = toInterface str :.: field
    toInterface (ConstExpr (BoolConst True () ()) ()) = litB True
    toInterface (ConstExpr (BoolConst False () ()) ()) = litB False
    toInterface (ConstExpr (IntConst x t () ()) ()) = litI (toInterface t) x
    toInterface (ConstExpr (FloatConst x () ()) ()) = litF x
    toInterface (ConstExpr (ComplexConst r i () ()) ()) = litC (toInterface $ ConstExpr r ()) (toInterface $ ConstExpr i ())
    toInterface (FunctionCall (Function name t Prefix) ps () ()) = Fun (toInterface t) name $ map toInterface ps
    toInterface (FunctionCall (Function name t Infix) ps () ()) = Binop (toInterface t) name $ map toInterface ps
    toInterface (AIR.Cast t e () ()) = Cast (toInterface t) (toInterface e)
    toInterface (SizeOf (Left t) () ()) = SizeofT $ toInterface t
    toInterface (SizeOf (Right e) () ()) = SizeofE $ toInterface e
    fromInterface (Var t name) = VarExpr (AIR.Variable name (fromInterface t) Value ()) ()
    fromInterface (Ptr t name) = VarExpr (AIR.Variable name (fromInterface t) AIR.Pointer ()) ()
    fromInterface (Lit (EBool True)) = ConstExpr (BoolConst True () ()) ()
    fromInterface (Lit (EBool False)) = ConstExpr (BoolConst False () ()) ()
    fromInterface (Lit (EInt t x)) = ConstExpr (IntConst x (fromInterface t) () ()) ()
    fromInterface (Lit (EFloat x)) = ConstExpr (FloatConst x () ()) ()
    fromInterface (Lit (EComplex
                        (fromInterface -> (ConstExpr r ()))
                        (fromInterface -> (ConstExpr i ())))) =
                        ConstExpr (ComplexConst r i () ()) ()
    fromInterface (Lit (EComplex _ _)) = error "Internal compiler error for LitC"
    fromInterface (Binop t name es) = FunctionCall (Function name (fromInterface t) Infix) (map fromInterface es) () ()
    fromInterface (Fun t name es) = FunctionCall (Function name (fromInterface t) Prefix) (map fromInterface es) () ()
    fromInterface (Cast t e) = AIR.Cast (fromInterface t) (fromInterface e) () ()
    fromInterface (SizeofE e) = SizeOf (Right $ fromInterface e) () ()
    fromInterface (SizeofT t) = SizeOf (Left $ fromInterface t) () ()
    fromInterface (arr :!: idx) = ArrayElem (fromInterface arr) (fromInterface idx) () ()
    fromInterface (NativeElem arr idx) = AIR.NativeElem (fromInterface arr) (fromInterface idx) () ()
    fromInterface (str :.: field) = StructField (fromInterface str) field () ()

instance Interface Prog where
    type Repr Prog = AIR.Program ()
    toInterface (Empty () ()) = Skip
    toInterface (AIR.Comment b s () ()) = Comment b s
    toInterface Assign{..} = toInterface lhs := toInterface rhs
    toInterface (ProcedureCall s k ps () ()) = Call s k (map toInterface ps)
    toInterface (Sequence ps () ()) = Seq (map toInterface ps)
    toInterface (Branch e b1 b2 () ()) = If (toInterface e) (toProg b1) (toProg b2)
    toInterface (Switch e alts () ()) = error "TODO: toInterface Switch"
    toInterface (SeqLoop e pe b () ()) = While (toProg pe) (toInterface e) (toProg b)
    toInterface (ParLoop v e i b () ()) = For (varName v) (toInterface e) i (toProg b)
    toInterface (BlockProgram b ()) = Block (map toInterface $ locals b) (toInterface $ blockBody b)
    fromInterface (Skip) = Empty () ()
    fromInterface (Comment b s) = AIR.Comment b s () ()
    fromInterface (lhs := rhs) = Assign (fromInterface lhs) (fromInterface rhs) () ()
    fromInterface (Call s k ps) = ProcedureCall s k (map fromInterface ps) () ()
    fromInterface (Seq ps) = Sequence (map fromInterface ps) () ()
    fromInterface (If e p1 p2) = Branch (fromInterface e) (toBlock p1) (toBlock p2) () ()
--    fromInterface (Switch scrut alts) = Switch (fromInterface scrut) (map toBlock alts) () () -- TODO: Add Switch in Prog.
    fromInterface (While pe e p) = SeqLoop (fromInterface e) (toBlock pe) (toBlock p) () ()
    fromInterface (For s e i p) = ParLoop
        (AIR.Variable s (NumType Unsigned S32) Value ()) (fromInterface e) i (toBlock p) () ()
    fromInterface (Block ds p) = BlockProgram (AIR.Block (map fromInterface ds) (fromInterface p) ()) ()

instance Interface Param where
    type Repr Param = ActualParameter ()
    toInterface (AIR.In e ()) = In (toInterface e)
    toInterface (AIR.Out e ()) = Out (toInterface e)
    toInterface (AIR.TypeParameter e AIR.Auto ()) = TypAuto (toInterface e)
    toInterface (AIR.TypeParameter e AIR.Scalar ()) = TypScalar (toInterface e)
    toInterface (AIR.FunParameter n k False ()) = Fn n k
    toInterface (AIR.FunParameter n k True ()) = FnAddr n k
    fromInterface (In e) = AIR.In (fromInterface e) ()
    fromInterface (Out e) = AIR.Out (fromInterface e) ()
    fromInterface (TypAuto e) = AIR.TypeParameter (fromInterface e) Auto ()
    fromInterface (TypScalar e) = AIR.TypeParameter (fromInterface e) Scalar ()
    fromInterface (Fn n k) = AIR.FunParameter n k False ()
    fromInterface (FnAddr n k) = AIR.FunParameter n k True ()

instance Interface Def where
    type Repr Def = Declaration ()
    toInterface (Declaration v (Just e) ()) = Init (toInterface v) (toInterface e)
    toInterface (Declaration v Nothing ()) = Def (toInterface v)
    fromInterface (Init v e) = Declaration (fromInterface v) (Just $ fromInterface e) ()
    fromInterface (Def v) = Declaration (fromInterface v) Nothing ()

instance Interface Block where
    type Repr Block = AIR.Block ()
    toInterface (AIR.Block ds p ()) = Bl (map toInterface ds) (toInterface p)
    fromInterface (Bl ds p) = AIR.Block (map fromInterface ds) (fromInterface p) ()

instance Interface Var where
    type Repr Var = AIR.Variable ()
    toInterface (AIR.Variable name typ Value ()) = Variable (toInterface typ) name
    toInterface (AIR.Variable name typ AIR.Pointer ()) = Pointer (toInterface typ) name
    fromInterface (Variable typ name) = AIR.Variable name (fromInterface typ) Value ()
    fromInterface (Pointer typ name) = AIR.Variable name (fromInterface typ) AIR.Pointer ()

toBlock :: Prog -> AIR.Block ()
toBlock (Block ds p) = AIR.Block (map fromInterface ds) (fromInterface p) ()
toBlock p = AIR.Block [] (fromInterface p) ()

toProg :: AIR.Block () -> Prog
toProg (AIR.Block [] p ()) = toInterface p
toProg (AIR.Block ds p ()) = Block (map toInterface ds) (toInterface p)

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
        | isArray t = Binop U32 "-" [litI U32 0,SizeofT t]
        | otherwise = SizeofT t
    t = case typeof arr of
        SizedArray _ e -> e
        _       -> error $ "Feldspar.Compiler.Imperative.Frontend.initArray: invalid type of array " ++ show arr ++ "::" ++ show (typeof arr)

assignProg :: Expr -> Expr -> Prog
assignProg inExp outExp = copyProg inExp [outExp]

freeArray :: Var -> Prog
freeArray arr = Call "freeArray" KNormal [Out $ varToExpr arr]

arrayLength :: Expr -> Expr
arrayLength arr
  | Just r <- chaseArray arr = litI U32 $ fromIntegral (upperBound r)
  | otherwise = Fun U32 "getLength" [arr]

chaseArray :: Expr -> Maybe (Range Length)
chaseArray e = go e []  -- TODO: Extend to handle x.member1.member2
  where go :: Expr -> [String] -> Maybe (Range Length)
        go (Var (SizedArray r _) _) [] | isSingleton r = Just r
        go (Ptr (SizedArray r _) _) [] | isSingleton r = Just r
        go (e :.: s) ss = go e (s:ss)
        go (Var (Struct fields) _) (s:_)
          | Just (SizedArray r _) <- lookup s fields 
          , isSingleton r = Just r
        go (Ptr (Struct fields) _) (s:_)
          | Just (SizedArray r _) <- lookup s fields 
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

spawn :: String -> [Var] -> Prog
spawn taskName vs = Call spawnName KTask allParams
  where
    spawnName = "spawn" ++ show (length vs)
    taskParam = FnAddr taskName KTask
    typeParams = map (TypAuto . vType) vs
    varParams = map (\v -> In $ Var (vType v) (vName v)) vs
    allParams = taskParam : concat (zipWith (\a b -> [a,b]) typeParams varParams)

run :: String -> [Var] -> Prog
run taskName vs = Call runName KTask allParams
  where
    runName = "run" ++ show (length vs)
    typeParams = map (TypAuto . vType) vs
    taskParam = Fn taskName KTask
    allParams = taskParam : typeParams
    
instance Show Type
  where
    show Void       = "void"
    show Boolean    = "bool"
    show Bit        = "bit"
    show Floating   = "float"
    show I8         = "int8"
    show I16        = "int16"
    show I32        = "int32"
    show I40        = "int40"
    show I64        = "int64"
    show U8         = "uint8"
    show U16        = "uint16"
    show U32        = "uint32"
    show U40        = "uint40"
    show U64        = "uint64"
    show (Complex t)    = "complexOf_" ++ show t
    show (UserType s)   = "userType_" ++ s
    show (NativeArray _ t) = show t ++ "*"
    show (SizedArray i t)
      | isSingleton i = "arrayOfSize_" ++ show (upperBound i) ++ "_" ++ show t
      | otherwise = "arrayOf_" ++ show t
    show (Struct fields)    = "struct_" ++ intercalate "_" (map (\(s,t) -> s ++ "_" ++ show t) fields)
    show (IVar t)   = "ivarOf_" ++ show t

instance HasType Expr
  where
    type TypeOf Expr = Type
    typeof = toInterface . typeof . fromInterface

instance HasType Var
  where
    type TypeOf Var = Type
    typeof = toInterface . typeof . fromInterface

intWidth :: Type -> Maybe Integer
intWidth I8  = Just 8
intWidth I16 = Just 16
intWidth I32 = Just 32
intWidth I40 = Just 40
intWidth I64 = Just 64
intWidth U8  = Just 8
intWidth U16 = Just 16
intWidth U32 = Just 32
intWidth U40 = Just 40
intWidth U64 = Just 64
intWidth _   = Nothing

intSigned :: Type -> Maybe Bool
intSigned I8  = Just True
intSigned I16 = Just True
intSigned I32 = Just True
intSigned I40 = Just True
intSigned I64 = Just True
intSigned U8  = Just False
intSigned U16 = Just False
intSigned U32 = Just False
intSigned U40 = Just False
intSigned U64 = Just False
intSigned _   = Nothing

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
litI32 n = Lit (EInt I32 n)

isArray :: Type -> Bool
isArray (SizedArray _ _) = True
isArray _ = False

vType :: Var -> Type
vType (Variable t _) = t
vType (Pointer t _) = t

vName :: Var -> String
vName (Variable _ s) = s
vName (Pointer _ s) = s

lName :: Expr -> String
lName (Var _ s) = s
lName (Ptr _ s) = s
lName (e :!: _) = lName e
lName (e :.: _) = lName e
lName e = error $ "Feldspar.Compiler.Imperative.Frontend.lName: invalid location: " ++ show e

varToExpr :: Var -> Expr
varToExpr (Variable t name) = Var t name
varToExpr (Pointer t name) = Ptr t name
