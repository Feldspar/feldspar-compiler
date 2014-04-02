{-# LANGUAGE ViewPatterns #-}
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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore.Interpretation where


import Control.Arrow
import Control.Monad.RWS
import Control.Applicative

import Data.Char (toLower)
import Data.List (intercalate, stripPrefix)

import Feldspar.Lattice (universal)
import Feldspar.Range (upperBound, isSingleton, singletonRange, fullRange)
import qualified Feldspar.Core.UntypedRepresentation as Ut

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.Representation (typeof, Block(..),
                                                    Type(..), Signedness(..),
                                                    Size(..), Variable(..),
                                                    Expression(..), ScalarType(..),
                                                    Declaration(..),
                                                    Program(..), Pattern(..),
                                                    Entity(..), StructMember(..))

import Feldspar.Compiler.Backend.C.Options (Options(..), Platform(..))

-- | Code generation monad
type CodeWriter = RWS Readers Writers States

data Readers = Readers { alias :: [(Integer, Expression ())] -- ^ variable aliasing
                       , backendOpts :: Options -- ^ Options for the backend.
                       }

initReader :: Options -> Readers
initReader = Readers []

data Writers = Writers { block    :: Block ()         -- ^ collects code within one block
                       , def      :: [Entity ()]      -- ^ collects top level definitions
                       , decl     :: [Declaration ()] -- ^ collects top level variable declarations
                       , params   :: [Variable ()]    -- ^ collects top level parameters
                       , epilogue :: [Program ()]     -- ^ collects postlude code (freeing memory, etc)
                       }

instance Monoid Writers
  where
    mempty      = Writers { block    = mempty
                          , def      = mempty
                          , decl     = mempty
                          , params   = mempty
                          , epilogue = mempty
                          }
    mappend a b = Writers { block    = mappend (block    a) (block    b)
                          , def      = mappend (def      a) (def      b)
                          , decl     = mappend (decl     a) (decl     b)
                          , params   = mappend (params   a) (params   b)
                          , epilogue = mappend (epilogue a) (epilogue b)
                          }

data States = States { fresh :: Integer -- ^ The first fresh variable id
                     }

initState :: States
initState = States 0

-- | Where to place the program result
type Location = Maybe (Expression ())

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

mkStructType :: [(String, Type)] -> Type
mkStructType trs = StructType n trs
  where
    n = intercalate "_" $ "s" : map (encodeType . snd) trs

compileTypeRep :: Ut.Type -> Type
compileTypeRep Ut.UnitType            = VoidType
compileTypeRep Ut.BoolType            = MachineVector 1 BoolType
compileTypeRep (Ut.IntType s n)       = MachineVector 1 (NumType s n)
compileTypeRep Ut.FloatType           = MachineVector 1 FloatType
compileTypeRep Ut.DoubleType          = MachineVector 1 DoubleType
compileTypeRep (Ut.ComplexType t)     = MachineVector 1 $ ComplexType (compileTypeRep t)
compileTypeRep (Ut.Tup2Type a b)           = mkStructType
        [ ("member1", compileTypeRep a)
        , ("member2", compileTypeRep b)
        ]
compileTypeRep (Ut.Tup3Type a b c)         = mkStructType
        [ ("member1", compileTypeRep a)
        , ("member2", compileTypeRep b)
        , ("member3", compileTypeRep c)
        ]
compileTypeRep (Ut.Tup4Type a b c d)       = mkStructType
        [ ("member1", compileTypeRep a)
        , ("member2", compileTypeRep b)
        , ("member3", compileTypeRep c)
        , ("member4", compileTypeRep d)
        ]
compileTypeRep (Ut.Tup5Type a b c d e)     = mkStructType
        [ ("member1", compileTypeRep a)
        , ("member2", compileTypeRep b)
        , ("member3", compileTypeRep c)
        , ("member4", compileTypeRep d)
        , ("member5", compileTypeRep e)
        ]
compileTypeRep (Ut.Tup6Type a b c d e f)   = mkStructType
        [ ("member1", compileTypeRep a)
        , ("member2", compileTypeRep b)
        , ("member3", compileTypeRep c)
        , ("member4", compileTypeRep d)
        , ("member5", compileTypeRep e)
        , ("member6", compileTypeRep f)
        ]
compileTypeRep (Ut.Tup7Type a b c d e f g) = mkStructType
        [ ("member1", compileTypeRep a)
        , ("member2", compileTypeRep b)
        , ("member3", compileTypeRep c)
        , ("member4", compileTypeRep d)
        , ("member5", compileTypeRep e)
        , ("member6", compileTypeRep f)
        , ("member7", compileTypeRep g)
        ]
compileTypeRep (Ut.MutType a)           = compileTypeRep a
compileTypeRep (Ut.RefType a)           = compileTypeRep a
compileTypeRep (Ut.ArrayType rs a)      = ArrayType rs $ compileTypeRep a
compileTypeRep (Ut.MArrType rs a)       = ArrayType rs $ compileTypeRep a
compileTypeRep (Ut.ParType a)           = compileTypeRep a
compileTypeRep (Ut.ElementsType a)      = ArrayType fullRange $ compileTypeRep a
compileTypeRep (Ut.IVarType a)          = IVarType $ compileTypeRep a
compileTypeRep (Ut.FunType _ b)         = compileTypeRep b
compileTypeRep (Ut.FValType a)          = IVarType $ compileTypeRep a
compileTypeRep typ                      = error $ "compileTypeRep: missing " ++ show typ  -- TODO

-- | Construct a variable.
mkVar :: Type -> Integer -> Expression ()
mkVar t i = varToExpr $ mkNamedVar "v" t i

-- | Construct a named variable.
mkNamedVar :: String -> Type -> Integer -> Variable ()
mkNamedVar base t i = Variable t $ base ++ if i < 0 then "" else show i

-- | Construct a named pointer.
mkNamedRef :: String -> Type -> Integer -> Variable ()
mkNamedRef base t i = Variable (Pointer t) $ base ++ if i < 0 then "" else show i

-- | Construct a pointer.
mkRef :: Type -> Integer -> Expression ()
mkRef t i = varToExpr $ mkNamedRef "v" t i

mkVariable :: Type -> Integer -> Variable ()
mkVariable = mkNamedVar "v"

mkPointer :: Type -> Integer -> Variable ()
mkPointer = mkNamedRef "v"

freshId :: CodeWriter Integer
freshId = do
  s <- get
  let v = fresh s
  put (s {fresh = v + 1})
  return v

freshVar :: String -> Ut.Type -> CodeWriter (Expression ())
freshVar base t = do
  v <- varToExpr . mkNamedVar base (compileTypeRep t) <$> freshId
  declare v
  return v

declare :: Expression () -> CodeWriter ()
declare (VarExpr v@(Variable{})) = tellDeclWith True [Declaration v Nothing]
declare expr      = error $ "declare: cannot declare expression: " ++ show expr

declareAlias :: Expression () -> CodeWriter ()
declareAlias (VarExpr v@(Variable{})) = tellDeclWith False [Declaration v Nothing]
declareAlias expr      = error $ "declareAlias: cannot declare expression: " ++ show expr

initialize :: Expression () -> Expression () -> CodeWriter ()
initialize (VarExpr v@(Variable{})) e = tellDeclWith True [Declaration v (Just e)]
initialize expr      _ = error $ "initialize: cannot declare expression: " ++ show expr

tellDef :: [Entity ()] -> CodeWriter ()
tellDef es = tell $ mempty {def = es}

tellProg :: [Program ()] -> CodeWriter ()
tellProg [BlockProgram b@(Block [] _)] = tell $ mempty {block = b}
tellProg ps = tell $ mempty {block = toBlock $ Sequence ps}

tellDeclWith :: Bool -> [Declaration ()] -> CodeWriter ()
tellDeclWith free ds = do
    rs <- ask
    let frees | free = freeArrays ds ++ freeIVars ds
              | otherwise = []
        opts = backendOpts rs
        defs = getTypes opts ds
        code | varFloating $ platform opts = mempty {decl=ds, epilogue = frees, def = defs}
             | otherwise = mempty {block = Block ds Empty,
                                   epilogue = frees, def = defs}
    tell code

encodeType :: Type -> String
encodeType = go
  where
    go VoidType            = "void"
    go (MachineVector 1 t) = goScalar t
    go (Pointer t)         = "ptr_" ++ go t
    go (AliasType _ s)     = s
    go (IVarType t)        = go t
    go (NativeArray _ t)   = go t
    go (StructType n _)    = n
    go (ArrayType l t)     = intercalate "_" ["arr", go t, if isSingleton l
                                                            then show (upperBound l)
                                                            else "UD"
                                             ]
    goScalar BoolType          = "bool"
    goScalar BitType           = "bit"
    goScalar FloatType         = "float"
    goScalar DoubleType        = "double"
    goScalar (NumType s w)     = map toLower (show s) ++ show w
    goScalar (ComplexType t)   = "complex" ++ go t

-- Almost the inverse of encodeType. Some type encodings are lossy so
-- they are impossible to recover.
decodeType :: String -> [Type]
decodeType s = goL s []
  where
    goL [] acc = reverse acc
    goL s acc = goL rest' (out:acc)
       where (out, rest) = go s
             rest' = case rest of
                      '_':t -> t
                      _     -> rest

    go (stripPrefix "void"     -> Just t) = (VoidType, t)
    go (stripPrefix "bool"     -> Just t) = (MachineVector 1 BoolType, t)
    go (stripPrefix "bit"      -> Just t) = (MachineVector 1 BitType, t)
    go (stripPrefix "float"    -> Just t) = (MachineVector 1 FloatType, t)
    go (stripPrefix "double"   -> Just t) = (MachineVector 1 DoubleType, t)
    go (stripPrefix "unsigned" -> Just t) = (MachineVector 1 (NumType Unsigned w), t')
     where (w, t') = decodeSize t
    go (stripPrefix "signed"   -> Just t) = (MachineVector 1 (NumType Signed w), t')
     where (w, t') = decodeSize t
    go (stripPrefix "complex"  -> Just t) = (MachineVector 1 (ComplexType tn), t')
     where (tn, t') = go t
    go (stripPrefix "ptr_"     -> Just t) = (Pointer tt, t')
     where (tt, t') = go t
-- Lossy encodings left out:
--    go (AliasType _ s)   = s
--    go (IVarType t)      = go t
--    go (NativeArray _ t) = go t
    go h@('s':'_':t) = (StructType h' $ zipWith mkMember [1..] ts, t'')
       where mkMember n t = ("member" ++ show n, t)
             (ts, t'') = structGo t []
             structGo [] acc = (reverse acc, [])
             structGo s  acc = if "_" == take 1 s' && not (likelyDim s')
                                 then structGo (drop 1 s') (ts:acc)
                                 else (reverse (ts:acc), s')
                      where (ts, s') = go s
             -- There might be dangling size encodings at the end of h
             -- because of a struct of arrays inside an array. The right
             -- h for this nest level is the part of h that is not
             -- intersecting with t''.
             h' = take (length h - length t'') h
    go (stripPrefix "arr_"     -> Just t) = (ArrayType d tt, t'')
      where (tt, ('_':t')) = go t
            (d, t'') = decodeDim t'
    go s = error ("decodeType: " ++ s)

    decodeSize (stripPrefix "S32" -> Just t) = (S32, t)
    decodeSize (stripPrefix "S8"  -> Just t) = (S8, t)
    decodeSize (stripPrefix "S16" -> Just t) = (S16, t)
    decodeSize (stripPrefix "S40" -> Just t) = (S40, t)
    decodeSize (stripPrefix "S64" -> Just t) = (S64, t)

    decodeDim ('_':t)     = (universal, t)
    decodeDim ('U':'D':t) = (universal, t)
    decodeDim ('1':t)     = (singletonRange 1, t)
    decodeDim e           = error ("decodeDim: " ++ e)

    likelyDim (stripPrefix "_UD" -> Just _)                          = True
    likelyDim ('_':n:_) | [(_,_)] <- reads [n] :: [(Integer,String)] = True
    likelyDim _                                                      = False

getTypes :: Options -> [Declaration ()] -> [Entity ()]
getTypes _ defs = concatMap mkDef comps
  where
    comps = filter isComposite' $ map (typeof . dVar) defs
    -- There are other composite types that are not flagged as such by this
    -- version of isComposite, so keep it private.
    isComposite' :: Type -> Bool
    isComposite' (StructType {}) = True
    isComposite' (Pointer t)     = isComposite' t
    isComposite' e               = isArray e
    mkDef (StructType n members)
      =  concatMap (mkDef . snd) members
      ++ [StructDef n (map (uncurry StructMember) members)]
    mkDef (ArrayType _ typ) = mkDef typ
    mkDef (Pointer typ)     = mkDef typ
    mkDef _                 = []

assign :: Location -> Expression () -> CodeWriter ()
assign (Just tgt) src = tellProg [if tgt == src then Empty else copyProg (Just tgt) [src]]
assign _          _   = return ()

-- | Like 'listen', but also prevents the program from being written in the
-- monad.
confiscateBlock :: CodeWriter a -> CodeWriter (a, Block ())
confiscateBlock m
    = liftM (second block)
    $ censor (\rec -> rec {block = mempty})
    $ listen m

-- | Like 'listen', but also catches writer things and prevents the program
-- from being written in the monad.
confiscateBigBlock :: CodeWriter a -> CodeWriter ((a, Writers), Block ())
confiscateBigBlock m
    = liftM (\c -> (c, block $ snd c))
    $ censor (\rec -> rec {block = mempty, decl = mempty, epilogue = mempty})
    $ listen m

withAlias :: Integer -> Expression () -> CodeWriter a -> CodeWriter a
withAlias v0 expr =
  local (\e -> e {alias = (v0,expr) : alias e})

isVariableOrLiteral :: Ut.UntypedFeld -> Bool
isVariableOrLiteral (Ut.In Ut.Literal{})  = True
isVariableOrLiteral (Ut.In Ut.Variable{}) = True
isVariableOrLiteral _                     = False

isComposite :: Type -> Bool
isComposite ArrayType{}   = True
isComposite NativeArray{} = True
isComposite StructType{}  = True
isComposite _             = False
