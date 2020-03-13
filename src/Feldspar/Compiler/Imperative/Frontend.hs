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
{-# LANGUAGE ViewPatterns #-}

module Feldspar.Compiler.Imperative.Frontend where

import Feldspar.Core.UntypedRepresentation (Fork(..))
import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Backend.C.Options

import Feldspar.Range
import Feldspar.Core.Types (Length)

import Data.Char (toLower)
import Data.List (intercalate, stripPrefix)

toBlock :: Program () -> Block ()
toBlock (BlockProgram b) = b
toBlock p                = Block [] p

toProg :: Block () -> Program ()
toProg (Block [] p) = p
toProg e = BlockProgram e

-- | Where to place the program result
type Location = Maybe (Expression ())

-- | Copies expressions into a destination. If the destination is
-- an array the arguments are appended into the destination.
copyProg :: Location -> [Expression ()] -> Program ()
copyProg _ []      = error "copyProg: missing source parameter."
copyProg Nothing _ = Empty
copyProg (Just outExp) inExp =
  case inExp of
    [x] | outExp == x -> Empty
    _                 -> deepCopy outExp inExp

-- | Expand copying of aggregate data to copying of components
deepCopy :: Expression () -> [Expression ()] -> Program ()
deepCopy arg1 [arg2]
  | arg1 == arg2
  = Empty

  | StructType _ fts <- typeof arg2
  = Sequence $ map (deepCopyField . fst) fts

  | not (isArray (typeof arg1) || isNativeArray (typeof arg1))
  = Assign arg1 arg2
    where deepCopyField fld = deepCopy (StructField arg1 fld) [StructField arg2 fld]

deepCopy arg1 args
  | isArray (typeof arg1) || isNativeArray (typeof arg1)
  = Assign arg1 $ fun (typeof arg1) "copy" (arg1 : args)

deepCopy _ _ = error "Multiple non-array arguments to copy"

flattenCopy :: Expression () -> [Expression ()] -> [Expression ()] ->
               Expression () -> [Program ()]
flattenCopy _ [] [] _ = []
flattenCopy dst (t:ts) (l:ls) cLen = call "copyArrayPos" [ValueParameter dst, ValueParameter cLen, ValueParameter t]
                                   : flattenCopy dst ts ls (ePlus cLen l)

ePlus :: Expression () -> Expression () -> Expression ()
ePlus (ConstExpr (IntConst 0 _)) e = e
ePlus e (ConstExpr (IntConst 0 _)) = e
ePlus e1 e2                        = binop (1 :# (NumType Signed S32)) "+" e1 e2

-- | Lower array copy
lowerCopy :: Options -> Type -> Expression () -> [Expression ()] -> [Program ()]
lowerCopy _ ArrayType{} arg1 ins'@(in1:ins)
  | [ConstExpr ArrayConst{..}] <- ins'
  = initArray (Just arg1) (litI32 $ toInteger $ length arrayValues)
    : zipWith (\i c -> Assign (ArrayElem arg1 [litI32 i]) (ConstExpr c)) [0..] arrayValues

  | otherwise
  = [ initArray (Just arg1) expDstLen, copyFirstSegment ] ++
      flattenCopy arg1 ins argnLens arg1len
    where expDstLen = foldr ePlus (litI32 0) aLens
          copyFirstSegment = if arg1 == in1
                                then Empty
                                else call "copyArray" [ ValueParameter arg1
                                                      , ValueParameter in1]
          aLens@(arg1len:argnLens) = map arrayLength ins'
lowerCopy opts NativeArray{} arg1 [arg2]
  | l@(ConstExpr (IntConst n _)) <- arrayLength arg2
  = if n < safetyLimit opts
      then initArray (Just arg1) l:map (\i -> Assign (ArrayElem arg1 [litI32 i]) (ArrayElem arg2 [litI32 i])) [0..(n-1)]
      else error $ unlines ["Frontend.lowerCopy: array size (" ++ show n ++ ") too large", show arg1, show arg2]
lowerCopy _ t e es = error $ "Frontend.lowerCopy: funny type (" ++ show t ++ ") or destination\n"
                                ++ show e ++ "\nor arguments\n"
                                ++ (unlines $ map show es)

-- | General array initialization
mkInitArr
    :: String         -- ^ Name of initialization function (e.g. \"initArray\")
    -> Location       -- ^ Array location
    -> Expression ()  -- ^ Array length
    -> Program ()
mkInitArr _    Nothing    _   = Empty
mkInitArr name (Just arr) len
  | isNativeArray arrType
  = Empty
  | otherwise
  = Assign arr $ fun arrType name [arr, sz, len]
   where
    arrType = typeof arr
    sz | isArray t' = binop (1 :# (NumType Unsigned S32)) "-" (litI32 0) t
       | otherwise  = t
    t = SizeOf t'
    t' = go $ arrType
    go (ArrayType _ e) = e
    go _               = error $ "Feldspar.Compiler.Imperative.Frontend."
                              ++ name ++ ": invalid type of array " ++ show arr
                              ++ "::" ++ show (typeof arr)

-- | Initialize an array using \"initArray\"
initArray
    :: Location       -- ^ Array location
    -> Expression ()  -- ^ Array length
    -> Program ()
initArray = mkInitArr "initArray"

-- | Generate a call to free an array represented as a variable
freeArray :: Variable () -> Program ()
freeArray arr = call "freeArray" [ValueParameter $ varToExpr arr]

-- | Generate 'freeArray' calls for all arrays in a list of declarations
freeArrays :: [Declaration ()] -> [Program ()]
freeArrays defs = map freeArray arrays
  where
    arrays = filter (isArray . typeof) $ map declVar defs

-- | Get the length of an array
arrayLength :: Expression () -> Expression ()
arrayLength arr
  | Just l <- staticArrayLength arr = litI32 $ fromIntegral l
  | otherwise = fun (1 :# (NumType Unsigned S32)) "getLength" [arr]

-- | If possible, return the static length of an array
staticArrayLength :: Expression t -> Maybe Length
staticArrayLength = go []  -- TODO: Extend to handle x.member1.member2
  where go :: [String] -> Expression t -> Maybe Length
        go []    (ConstExpr (ArrayConst l _)) = Just (fromIntegral $ length l)
        go []    (VarExpr (Variable (ArrayType r _) _)) | isSingleton r = Just (upperBound r)
        go []    (VarExpr (Variable (NativeArray (Just l) _) _)) = Just l
        go []    (Deref e) = go [] e -- TODO: this is questionable; we now look at an expression for the address of the array
        go ss    (StructField e s) = go (s:ss) e
        go ss    (AddrOf e) = go ss e
        go (s:_) (VarExpr (Variable (StructType _ fields) _))
          | Just (ArrayType r _) <- lookup s fields
          , isSingleton r = Just (upperBound r)
          | Just (NativeArray (Just l) _) <- lookup s fields
          = Just l
        go _ _ = Nothing

iVarInitCond :: Fork -> Expression () -> Program ()
iVarInitCond Future var = iVarInit var
iVarInitCond _      _   = Empty

iVarInit :: Expression () -> Program ()
iVarInit var = call "ivar_init" [ValueParameter var]

iVarGet :: Bool -> Expression () -> Expression () -> Program ()
iVarGet inTask loc ivar
    | isArray typ   = Assign loc
                    $ fun (typeof loc) (mangle inTask "ivar_get_array") [ loc, ivar ]
    | otherwise     = call (mangle inTask "ivar_get") [ TypeParameter typ
                                                       , ValueParameter (AddrOf loc)
                                                       , ValueParameter ivar]
      where
        typ = typeof loc
        mangle True  s = s
        mangle False s = s ++ "_nontask"

iVarPut :: Expression () -> Expression () -> Program ()
iVarPut ivar msg
    | isArray typ   = call "ivar_put_array" [ValueParameter ivar, ValueParameter  msg]
    | otherwise     = call "ivar_put" [TypeParameter typ, ValueParameter ivar, ValueParameter (AddrOf msg)]
      where
        typ = typeof msg

-- | Generate a call to free an IVar represented as a variable
iVarDestroy :: Variable () -> Program ()
iVarDestroy v = call "ivar_destroy" [ValueParameter $ AddrOf $ varToExpr v]

-- | Generate 'iVarDestroy' calls for all IVars in a list of declarations
freeIVars :: [Declaration ()] -> [Program ()]
freeIVars defs = map iVarDestroy ivars
  where
    ivars = filter (isIVar . typeof) $ map declVar defs

spawn :: Fork -> String -> [Variable ()] -> Program ()
spawn f taskName vs
 | f `elem` [None, Loop] = call taskName $ map mkV vs
 | otherwise = call spawnName allParams
  where
    mkV v = ValueParameter . varToExpr $ Variable (typeof v) (varName v)
    spawnName = "spawn" ++ show (length vs)
    taskParam = FunParameter taskName
    typeParams = map (TypeParameter . typeof) vs
    varParams = map (ValueParameter . mkv) vs
      where mkv v = VarExpr $ Variable (typeof v) (varName v)
    allParams = taskParam : concat (zipWith (\a b -> [a,b]) typeParams varParams)

run :: String -> [Variable ()] -> Program ()
run taskName vs = call runName allParams
  where
    runName = "run" ++ show (length vs)
    typeParams = map (TypeParameter . typeof) vs
    taskParam = FunParameter taskName
    allParams = taskParam : typeParams

intWidth :: Type -> Maybe Integer
intWidth (1 :# (NumType _ S8))             = Just 8
intWidth (1 :# (NumType _ S16))            = Just 16
intWidth (1 :# (NumType _ S32))            = Just 32
intWidth (1 :# (NumType _ S40))            = Just 40
intWidth (1 :# (NumType _ S64))            = Just 64
intWidth _                                 = Nothing

intSigned :: Type -> Maybe Bool
intSigned (1 :# (NumType Unsigned _))            = Just False
intSigned (1 :# (NumType Signed _))              = Just True
intSigned _                                      = Nothing

litF :: Float -> Expression t
litF n = ConstExpr (FloatConst n)

litD :: Double -> Expression t
litD n = ConstExpr (DoubleConst n)

litB :: Bool -> Expression ()
litB b = ConstExpr (BoolConst b)

litC :: Constant () -> Constant () -> Expression ()
litC r i = ConstExpr (ComplexConst r i)

litI :: Type -> Integer -> Expression ()
litI (_ :# t) n = ConstExpr (IntConst n t)

litI32 :: Integer -> Expression ()
litI32 = litI (1 :# (NumType Unsigned S32))

isComplex :: Type -> Bool
isComplex (_ :# ComplexType{})            = True
isComplex _                               = False

isFloat :: Type -> Bool
isFloat (_ :# FloatType{})            = True
isFloat _                             = False

isArray :: Type -> Bool
isArray ArrayType{}                   = True
isArray _                             = False

isNativeArray :: Type -> Bool
isNativeArray NativeArray{}                   = True
isNativeArray _                               = False

isIVar :: Type -> Bool
isIVar IVarType{} = True
isIVar _              = False

isPointer :: Type -> Bool
isPointer (_ :# Pointer{})            = True
isPointer _                           = False

isVarExpr :: Expression () -> Bool
isVarExpr VarExpr{} = True
isVarExpr _         = False

isComposite :: Type -> Bool
isComposite ArrayType{}   = True
isComposite NativeArray{} = True
isComposite StructType{}  = True
isComposite _             = False

-- | Does the parameters allow a fast/cheap (in register) return.
canFastReturn :: Type -> Bool
canFastReturn t
  | not $ isComposite t
  , not $ isPointer t -- Conservative, pointers are handled the regular way.
  , not $ isIVar t      = True
canFastReturn _         = False

containsNativeArray :: Type -> Bool
containsNativeArray t = any (isNativeArray . snd) $ flattenStructs t

-- | Returns a list of access functions and types for the leaves of the struct tree of the type
flattenStructs :: Type -> [(Expression () -> Expression (), Type)]
flattenStructs (StructType _ fts) = [(\ e -> af $ StructField e fname, t') | (fname,t) <- fts, (af, t') <- flattenStructs t]
flattenStructs t = [(id, t)]

hasReference :: Type -> Bool
hasReference ArrayType{}                 = True
hasReference NativeArray{}               = True -- TODO: Reconsider this safe approximation if we start using native arrays for performance
hasReference (_ :# Pointer{})            = True
hasReference IVarType{}                  = True
hasReference (StructType _ fs)           = any (hasReference . snd) fs
hasReference _                           = False

varToExpr :: Variable t -> Expression t
varToExpr = VarExpr

exprToVar :: Expression () -> Variable ()
exprToVar (VarExpr v) = v
exprToVar (Deref e)   = exprToVar e
exprToVar e           = error $ "Frontend.exprToVar: Unexpected variable:" ++ show e

binop :: Type -> String -> Expression () -> Expression () -> Expression ()
binop t n e1 e2 = fun t n [e1, e2]

fun :: Type -> String -> [Expression ()] -> Expression ()
fun t n = FunctionCall (Function n t)

mkIf :: Expression () -> Block () -> Maybe (Block ()) -> Program ()
mkIf ce tb Nothing   = Switch ce [(Pat (litB True), tb)]
mkIf ce tb (Just eb) = Switch ce [(Pat (litB True), tb), (Pat (litB False), eb)]

call :: String -> [ActualParameter ()] -> Program ()
call = ProcedureCall

for :: ParType -> Variable () -> Expression () -> Expression () -> Expression () -> Block () -> Program ()
for _ _ _ _ _ (Block [] (Sequence ps)) | all (== Empty) ps = Empty
for p n s e i b = ParLoop p n s e i b

while :: Block () -> Expression () -> Block () -> Program ()
while p e = SeqLoop e p

{-
Encoded format is:

<type tag>_<inner_type_tag(s)>

Where the type tag is some unique prefix except for the scalar
types. The tag for StructTypes include the number of elements in the
struct to simplify the job for decodeType.

-}

encodeType :: Type -> String
encodeType = go
  where
    go VoidType              = "void"
    -- Machine vectors do not change memory layout, so keep internal.
    go (_ :# t)              = goScalar t
    go (IVarType t)          = "i_" ++ go t
    go (NativeArray _ t)     = "narr_" ++ go t
    go (StructType n _)      = n
    go (ArrayType _ t)       = "arr_" ++ go t
    goScalar BoolType        = "bool"
    goScalar BitType         = "bit"
    goScalar FloatType       = "float"
    goScalar DoubleType      = "double"
    goScalar (NumType s w)   = map toLower (show s) ++ show w
    goScalar (ComplexType t) = "complex_" ++ go t
    goScalar (Pointer t)     = "ptr_" ++ go t

-- Almost the inverse of encodeType. Some type encodings are lossy so
-- they are impossible to recover.
decodeType :: String -> [Type]
decodeType = goL []
  where
    goL acc [] = reverse acc
    goL acc s  = goL (out:acc) rest'
       where (out, rest) = go s
             rest' = case rest of
                      '_':t -> t
                      _     -> rest

    go (stripPrefix "void"     -> Just t) = (VoidType, t)
    go (stripPrefix "bool"     -> Just t) = (1 :# BoolType, t)
    go (stripPrefix "bit"      -> Just t) = (1 :# BitType, t)
    go (stripPrefix "float"    -> Just t) = (1 :# FloatType, t)
    go (stripPrefix "double"   -> Just t) = (1 :# DoubleType, t)
    go (stripPrefix "unsigned" -> Just t) = (1 :# (NumType Unsigned w), t')
     where (w, t') = decodeSize t
    go (stripPrefix "signed"   -> Just t) = (1 :# (NumType Signed w), t')
     where (w, t') = decodeSize t
    go (stripPrefix "complex"  -> Just t) = (1 :# (ComplexType tn), t')
     where (tn, t') = go t
    go (stripPrefix "ptr_"     -> Just t) = (1 :# (Pointer tt), t')
     where (tt, t') = go t
    go (stripPrefix "i_"       -> Just t) = (IVarType tt, t')
     where (tt, t') = go t
    go (stripPrefix "narr_"    -> Just t) = (NativeArray Nothing tt, t')
     where (tt, t') = go t
    go h@('s':'_':t) = (StructType h' $ zipWith mkMember [1..] ts, t'')
       where mkMember n t = ("member" ++ show n, t)
             Just (n, t') = decodeLen t
             (ts, t'') = structGo n t' []
             structGo 0 s acc = (reverse acc, s)
             structGo n ('_':s) acc = structGo (n - 1) s' (ts:acc)
                      where (ts, s') = go s
             h' = take (length h - length t'') h
    go (stripPrefix "arr_"     -> Just t) = (ArrayType fullRange tt, t')
      where (tt, t') = go t
    go s = error ("decodeType: " ++ s)

    decodeSize (stripPrefix "S32" -> Just t) = (S32, t)
    decodeSize (stripPrefix "S8"  -> Just t) = (S8, t)
    decodeSize (stripPrefix "S16" -> Just t) = (S16, t)
    decodeSize (stripPrefix "S40" -> Just t) = (S40, t)
    decodeSize (stripPrefix "S64" -> Just t) = (S64, t)

    decodeLen e
      | [p@(_,_)] <- reads e :: [(Int,String)]
      = Just p
      | otherwise = Nothing
