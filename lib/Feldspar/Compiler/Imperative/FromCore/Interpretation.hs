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
import Data.List (intercalate, stripPrefix, nub)
import Data.Tree
import Data.Maybe

import Feldspar.Range (upperBound, fullRange)
import qualified Feldspar.Core.UntypedRepresentation as Ut

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.Representation (typeof, Block(..),
                                                    Type(..), Signedness(..),
                                                    Size(..), Variable(..),
                                                    Expression(..), ScalarType(..),
                                                    Declaration(..),
                                                    Program(..),
                                                    ActualParameter(..),
                                                    Entity(..), StructMember(..))

import Feldspar.Compiler.Backend.C.Options (Options(..), Platform(..))

-- | Code generation monad
type CodeWriter = RWS Readers Writers States

data Readers = Readers { alias :: [(Integer, MultiExpr)] -- ^ variable aliasing
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
                     , stash :: Maybe Ut.UntypedFeld -- ^ Loop canary
                     }

initState :: States
initState = States 0 Nothing

-- | Where to place the program result
type MultiExpr = Tree (Maybe (Expression ()))
type Location  = MultiExpr

mkLoc :: Expression () -> Location
mkLoc e = Node (Just e) []

leaf :: Location -> Maybe (Expression ())
leaf (Node e []) = e
leaf _           = Nothing

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

mkStructType :: [(String, Type)] -> Type
mkStructType trs = StructType n trs
  where
    n = "s_" ++ intercalate "_" (show (length trs):map (encodeType . snd) trs)

compileTypeRep :: Options -> Ut.Type -> Type
compileTypeRep _   Ut.UnitType            = VoidType
compileTypeRep _   Ut.BoolType            = MachineVector 1 BoolType
compileTypeRep _   (Ut.IntType s n)       = MachineVector 1 (NumType s n)
compileTypeRep _   Ut.FloatType           = MachineVector 1 FloatType
compileTypeRep _   Ut.DoubleType          = MachineVector 1 DoubleType
compileTypeRep opt (Ut.ComplexType t)     = MachineVector 1 $ ComplexType (compileTypeRep opt t)
compileTypeRep opt (Ut.Tup2Type a b)           = mkStructType
        [ ("member1", compileTypeRep opt a)
        , ("member2", compileTypeRep opt b)
        ]
compileTypeRep opt (Ut.Tup3Type a b c)         = mkStructType
        [ ("member1", compileTypeRep opt a)
        , ("member2", compileTypeRep opt b)
        , ("member3", compileTypeRep opt c)
        ]
compileTypeRep opt (Ut.Tup4Type a b c d)       = mkStructType
        [ ("member1", compileTypeRep opt a)
        , ("member2", compileTypeRep opt b)
        , ("member3", compileTypeRep opt c)
        , ("member4", compileTypeRep opt d)
        ]
compileTypeRep opt (Ut.Tup5Type a b c d e)     = mkStructType
        [ ("member1", compileTypeRep opt a)
        , ("member2", compileTypeRep opt b)
        , ("member3", compileTypeRep opt c)
        , ("member4", compileTypeRep opt d)
        , ("member5", compileTypeRep opt e)
        ]
compileTypeRep opt (Ut.Tup6Type a b c d e f)   = mkStructType
        [ ("member1", compileTypeRep opt a)
        , ("member2", compileTypeRep opt b)
        , ("member3", compileTypeRep opt c)
        , ("member4", compileTypeRep opt d)
        , ("member5", compileTypeRep opt e)
        , ("member6", compileTypeRep opt f)
        ]
compileTypeRep opt (Ut.Tup7Type a b c d e f g) = mkStructType
        [ ("member1", compileTypeRep opt a)
        , ("member2", compileTypeRep opt b)
        , ("member3", compileTypeRep opt c)
        , ("member4", compileTypeRep opt d)
        , ("member5", compileTypeRep opt e)
        , ("member6", compileTypeRep opt f)
        , ("member7", compileTypeRep opt g)
        ]
compileTypeRep opt (Ut.MutType a)           = compileTypeRep opt a
compileTypeRep opt (Ut.RefType a)           = compileTypeRep opt a
compileTypeRep opt (Ut.ArrayType rs a)
 | useNativeArrays opt = NativeArray (Just $ upperBound rs) $ compileTypeRep opt a
 | otherwise           = ArrayType rs $ compileTypeRep opt a
compileTypeRep opt (Ut.MArrType rs a)
 | useNativeArrays opt = NativeArray (Just $ upperBound rs) $ compileTypeRep opt a
 | otherwise           = ArrayType rs $ compileTypeRep opt a
compileTypeRep opt (Ut.ParType a)           = compileTypeRep opt a
compileTypeRep opt (Ut.ElementsType a)
 | useNativeArrays opt = NativeArray Nothing $ compileTypeRep opt a
 | otherwise           = ArrayType fullRange $ compileTypeRep opt a
compileTypeRep opt (Ut.IVarType a)          = IVarType $ compileTypeRep opt a
compileTypeRep opt (Ut.FunType _ b)         = compileTypeRep opt b
compileTypeRep opt (Ut.FValType a)          = IVarType $ compileTypeRep opt a
compileTypeRep _   typ                      = error $ "compileTypeRep: missing " ++ show typ  -- TODO

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

freshVar :: Options -> String -> Ut.Type -> CodeWriter MultiExpr
freshVar opt base t = do
  v <- varToExpr . mkNamedVar base (compileTypeRep opt t) <$> freshId
  declare v
  return $ mkLoc v

freshAlias :: Expression () -> CodeWriter MultiExpr
freshAlias e = do i <- freshId
                  let vexp = varToExpr $ mkNamedVar "e" (typeof e) i
                  declareAlias vexp
                  return $ mkLoc vexp

-- | Create a fresh variable aliasing some other variable and
-- initialize it to the parameter.
freshAliasInit :: Expression () -> CodeWriter MultiExpr
freshAliasInit e = do vexp <- freshAlias e
                      tellProg [Assign (leaf vexp) e]
                      return vexp

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
        defs = getTypes ds
        code | varFloating $ platform opts = mempty {decl=ds, epilogue = frees, def = defs}
             | otherwise = mempty {block = Block ds Empty,
                                   epilogue = frees, def = defs}
    tell code

{-
Encoded format is:

<type tag>_<inner_type_tag(s)>

Where the type tag is some unique prefix except for the scalar
types. The tag for StructTypes include the number of elements in the
struct to simplify the job for decodeType, and AliasType does the same
thing with encoding the length of the name of the aliased type.

-}

encodeType :: Type -> String
encodeType = go
  where
    go VoidType              = "void"
    -- Machine vectors do not change memory layout, so keep internal.
    go (MachineVector _ t)   = goScalar t
    go (Pointer t)           = "ptr_" ++ go t
    go (AliasType t s)       = "a_" ++ show (length s) ++ "_" ++ s ++ "_" ++ go t
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
    go (stripPrefix "a_"       -> Just t) = (AliasType tt s', t'')
     where Just (n, '_':t') = decodeLen t
           s' = take n t'
           (tt, t'') = go $ drop n t'
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

getTypes :: [Declaration ()] -> [Entity ()]
getTypes defs = nub $ concatMap mkDef comps
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

assign :: Location -> MultiExpr -> CodeWriter ()
assign lhs@(Node _ []) rhs@(Node _ [])                  = tellProg [copyProg lhs [rhs]]
assign (Node _ ls) (Node _ rs) | length ls == length rs = zipWithM_ assign ls rs
assign lhs rhs = error $ unwords ["assign:","malformed trees",show lhs,show rhs]

-- | Copies expressions into a destination. If the destination is
-- a non-scalar the arguments are appended to the destination.
copyProg :: Location -> [MultiExpr] -> Program ()
copyProg _ [] = error "copyProg: missing source parameter."
copyProg (leaf -> Nothing) _ = Empty
copyProg outExp [inExp]
    | outExp == inExp = Empty
copyProg (leaf -> Just outExp) inExps =
    call "copy" (map ValueParameter (outExp:(mapMaybe leaf inExps)))


shallowAssign :: Location -> Expression () -> CodeWriter ()
shallowAssign loc@(leaf -> Just dst) src | dst /= src = tellProg [Assign (Just dst) src]
shallowAssign _                      _                = return ()

shallowCopyWithRefSwap :: Expression () -> Expression () -> CodeWriter ()
shallowCopyWithRefSwap dst src
  | dst /= src
  = case filter (hasReference . snd) $ flattenStructs $ typeof dst of
      [] -> tellProg [Assign (Just dst) src]
      arrs -> do temps <- sequence [freshAliasInit $ accF dst | (accF, _) <- arrs]
                 tellProg [Assign (Just dst) src]
                 tellProg [Assign (Just $ accF src) tmp | (Just tmp, (accF, _)) <- zip (map leaf temps) arrs]
  | otherwise = return ()

shallowCopyReferences :: Expression () -> Expression () -> CodeWriter ()
shallowCopyReferences dst src
  = tellProg [Assign (Just $ accF dst) (accF src)
                 | (accF, t) <- flattenStructs $ typeof dst, hasReference t]

{-
This function implements double buffering of state for imnmutable
seqential loops (forLoop and whileLoop).  The intention is to generate
the minimal amount of copying (especially deep array copies) while
also avoiding frequent references to data that can not be allocated in
registers.

The state of the loop is implemented by two variables so that the
loop body reads from one and writes to the other. At the end of the
body, the contents of the second (write) variable is shallow copied to
the first (read) variable. In order to avoid inadvertent sharing of
data referenced by pointers in the variables (array buffers, for
instance), the pointers in the state are swapped rather than just
copied so that the end up in the variable written to. Finally the read
variable is shallow copied to the result location.

There are some simplifying cases:
    - When the target lvalue loc is a variable, and thus cheap to
      access, it is reused as the read state variable
    - When the type of the state is scalar, so that assignment is
      atomic, only one state variable is used and it is both read and
      written to in the loop body, eliminating the shallow copy in the
      loop body.
The strategy implemented a compromise between different design constraints:
    - Avoid deep copying of arrays (that is what the double buffering is for)
    - Avoid shallow copying if possible
    - Avoid memory leaks of arrays and ivars
-}

mkDoubleBufferState :: Expression () -> Integer -> CodeWriter (Expression (), Expression ())
mkDoubleBufferState loc stvar
   = do stvar1 <- if isVarExpr loc || containsNativeArray (typeof loc)
                     then return loc
                     else do Just vexp <- leaf <$> freshAlias loc
                             shallowCopyReferences vexp loc
                             return vexp
        stvar2 <- if isComposite $ typeof loc
                     then do let vexp2 = mkVar (typeof loc) stvar
                             declare vexp2
                             return vexp2
                     else return stvar1
        return (stvar1, stvar2)


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

withAlias :: Integer -> MultiExpr -> CodeWriter a -> CodeWriter a
withAlias v0 expr = local (\e -> e {alias = (v0,expr) : alias e})

isVariableOrLiteral :: Ut.UntypedFeld -> Bool
isVariableOrLiteral (Ut.In Ut.Literal{})  = True
isVariableOrLiteral (Ut.In Ut.Variable{}) = True
isVariableOrLiteral _                     = False
