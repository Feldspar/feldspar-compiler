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
import Data.Maybe (isJust, fromJust)

import Language.Syntactic.Syntax hiding (result)
import Language.Syntactic.Traversal
import Language.Syntactic.Constraint
import Language.Syntactic.Constructs.Binding (VarId)

import Feldspar.Lattice (universal)
import Feldspar.Range (upperBound, isSingleton, singletonRange)
import Feldspar.Core.Types hiding (Type, ArrayType, BoolType, FloatType, DoubleType,
                                   ComplexType, IVarType, Signedness, Size)
import Feldspar.Core.Interpretation
import qualified Feldspar.Core.Types as Core
import qualified Feldspar.Core.Constructs.Binding as Core
import qualified Feldspar.Core.Constructs.Literal as Core

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

data Readers = Readers { alias :: [(VarId, Expression ())] -- ^ variable aliasing
                       , sourceInfo :: SourceInfo -- ^ Surrounding source info
                       , backendOpts :: Options -- ^ Options for the backend.
                       }

initReader :: Options -> Readers
initReader = Readers [] ""

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

data States = States { fresh :: VarId -- ^ The first fresh variable id
                     }

initState :: States
initState = States 0

-- | Where to place the program result
type Location = Maybe (Expression ())

-- | A minimal complete instance has to define either 'compileProgSym' or
-- 'compileExprSym'.
class Compile sub dom
  where
    compileProgSym
        :: sub a
        -> Info (DenResult a)
        -> Location
        -> Args (AST (Decor Info dom)) a
        -> CodeWriter ()
    compileProgSym = compileExprLoc

    compileExprSym
        :: sub a
        -> Info (DenResult a)
        -> Args (AST (Decor Info dom)) a
        -> CodeWriter (Expression ())
    compileExprSym = compileProgFresh

instance (Compile sub1 dom, Compile sub2 dom) =>
    Compile (sub1 :+: sub2) dom
  where
    compileProgSym (InjL a) = compileProgSym a
    compileProgSym (InjR a) = compileProgSym a

    compileExprSym (InjL a) = compileExprSym a
    compileExprSym (InjR a) = compileExprSym a



-- | Implementation of 'compileExprSym' that assigns an expression to the given
-- location.
compileExprLoc :: Compile sub dom
    => sub a
    -> Info (DenResult a)
    -> Location
    -> Args (AST (Decor Info dom)) a
    -> CodeWriter ()
compileExprLoc a info loc args = do
    expr <- compileExprSym a info args
    assign loc expr

-- | Implementation of 'compileProgSym' that generates code into a fresh
-- variable.
compileProgFresh :: Compile sub dom
    => sub a
    -> Info (DenResult a)
    -> Args (AST (Decor Info dom)) a
    -> CodeWriter (Expression ())
compileProgFresh a info args = do
    loc <- freshVar "e" (infoType info) (infoSize info)
    compileProgSym a info (Just loc) args
    return loc

compileDecor :: Info a -> CodeWriter b -> CodeWriter b
compileDecor info action = do
    let src = infoSource info
    aboveSrc <- asks sourceInfo
    unless (null src || src==aboveSrc) $ tellProg [Comment True src]
    local (\env -> env{sourceInfo=src}) action

compileProgDecor :: Compile dom dom
    => Location
    -> Decor Info dom a
    -> Args (AST (Decor Info dom)) a
    -> CodeWriter ()
compileProgDecor result (Decor info a) args =
    compileDecor info $ compileProgSym a info result args

compileExprDecor :: Compile dom dom
    => Decor Info dom a
    -> Args (AST (Decor Info dom)) a
    -> CodeWriter (Expression ())
compileExprDecor (Decor info a) args =
    compileDecor info $ compileExprSym a info args

compileProg :: Compile dom dom =>
    Location -> ASTF (Decor Info dom) a -> CodeWriter ()
compileProg result = simpleMatch (compileProgDecor result)

compileExpr :: Compile dom dom => ASTF (Decor Info dom) a -> CodeWriter (Expression ())
compileExpr = simpleMatch compileExprDecor

-- Compile an expression and make sure that the result is stored in a variable
compileExprVar :: Compile dom dom => ASTF (Decor Info dom) a -> CodeWriter (Expression ())
compileExprVar e = do
    e' <- compileExpr e
    case e' of
        _ | isNearlyVar e' -> return e'
        _         -> do
            varId <- freshId
            let loc = varToExpr $ mkNamedVar "e" (typeof e') varId
            declare loc
            assign (Just loc) e'
            return loc
  where isNearlyVar VarExpr{}  = True
        isNearlyVar (Deref e)  = isNearlyVar e
        isNearlyVar (AddrOf e) = isNearlyVar e
        isNearlyVar _          = False

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

compileNumType :: Core.Signedness a -> BitWidth n -> ScalarType
compileNumType U N8      = NumType Unsigned S8
compileNumType S N8      = NumType Signed S8
compileNumType U N16     = NumType Unsigned S16
compileNumType S N16     = NumType Signed S16
compileNumType U N32     = NumType Unsigned S32
compileNumType S N32     = NumType Signed S32
compileNumType U N64     = NumType Unsigned S64
compileNumType S N64     = NumType Signed S64
compileNumType U NNative = NumType Unsigned S32  -- TODO
compileNumType S NNative = NumType Signed S32    -- TODO

mkStructType :: [(String, Type)] -> Type
mkStructType trs = StructType n trs
  where
    n = intercalate "_" $ "s" : map (encodeType . snd) trs

compileTypeRep :: TypeRep a -> Core.Size a -> Type
compileTypeRep UnitType _                = VoidType
compileTypeRep Core.BoolType _           = MachineVector 1 BoolType
compileTypeRep (IntType s n) _           = MachineVector 1 $ compileNumType s n
compileTypeRep Core.FloatType _          = MachineVector 1 FloatType
compileTypeRep Core.DoubleType _         = MachineVector 1 DoubleType
compileTypeRep (Core.ComplexType t) _    = MachineVector 1 $ ComplexType (compileTypeRep t (defaultSize t))
compileTypeRep (Tup2Type a b) (sa,sb)          = mkStructType
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        ]
compileTypeRep (Tup3Type a b c) (sa,sb,sc)        = mkStructType
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        , ("member3", compileTypeRep c sc)
        ]
compileTypeRep (Tup4Type a b c d) (sa,sb,sc,sd)      = mkStructType
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        , ("member3", compileTypeRep c sc)
        , ("member4", compileTypeRep d sd)
        ]
compileTypeRep (Tup5Type a b c d e) (sa,sb,sc,sd,se)    = mkStructType
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        , ("member3", compileTypeRep c sc)
        , ("member4", compileTypeRep d sd)
        , ("member5", compileTypeRep e se)
        ]
compileTypeRep (Tup6Type a b c d e f) (sa,sb,sc,sd,se,sf)  = mkStructType
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        , ("member3", compileTypeRep c sc)
        , ("member4", compileTypeRep d sd)
        , ("member5", compileTypeRep e se)
        , ("member6", compileTypeRep f sf)
        ]
compileTypeRep (Tup7Type a b c d e f g) (sa,sb,sc,sd,se,sf,sg) = mkStructType
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        , ("member3", compileTypeRep c sc)
        , ("member4", compileTypeRep d sd)
        , ("member5", compileTypeRep e se)
        , ("member6", compileTypeRep f sf)
        , ("member7", compileTypeRep g sg)
        ]
compileTypeRep (MutType a) _            = compileTypeRep a (defaultSize a)
compileTypeRep (RefType a) _            = compileTypeRep a (defaultSize a)
compileTypeRep (Core.ArrayType a) (rs :> es) = ArrayType rs $ compileTypeRep a es
compileTypeRep (MArrType a) (rs :> es)  = ArrayType rs $ compileTypeRep a es
compileTypeRep (ParType a) _            = compileTypeRep a (defaultSize a)
compileTypeRep (ElementsType a) (rs :> es) = ArrayType rs $ compileTypeRep a es
compileTypeRep (Core.IVarType a) _      = IVarType $ compileTypeRep a $ defaultSize a
compileTypeRep (FunType _ b) (_, sz)    = compileTypeRep b sz
compileTypeRep (FValType a) sz          = IVarType $ compileTypeRep a sz
compileTypeRep typ _                    = error $ "compileTypeRep: missing " ++ show typ  -- TODO

-- | Construct a variable.
mkVar :: Type -> VarId -> Expression ()
mkVar t i = varToExpr $ mkNamedVar "v" t i

-- | Construct a named variable.
mkNamedVar :: String -> Type -> VarId -> Variable ()
mkNamedVar base t i = Variable t $ base ++ if i < 0 then "" else show i

-- | Construct a named pointer.
mkNamedRef :: String -> Type -> VarId -> Variable ()
mkNamedRef base t i = Variable (Pointer t) $ base ++ if i < 0 then "" else show i

-- | Construct a pointer.
mkRef :: Type -> VarId -> Expression ()
mkRef t i = varToExpr $ mkNamedRef "v" t i

mkVariable :: Type -> VarId -> Variable ()
mkVariable = mkNamedVar "v"

mkPointer :: Type -> VarId -> Variable ()
mkPointer = mkNamedRef "v"

freshId :: CodeWriter VarId
freshId = do
  s <- get
  let v = fresh s
  put (s {fresh = v + 1})
  return v

freshVar :: String -> TypeRep a -> Core.Size a -> CodeWriter (Expression ()) -- TODO take just info instead of TypeRep and Size?
freshVar base t sz = do
  v <- varToExpr . mkNamedVar base (compileTypeRep t sz) <$> freshId
  declare v
  return v

freshAlias :: Expression () -> CodeWriter (Expression ())
freshAlias e = do i <- freshId
                  let vexp = varToExpr $ mkNamedVar "e" (typeof e) i
                  declareAlias vexp
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
    decodeDim e
      | [(n,t)] <- reads e :: [(Integer,String)]
      = (singletonRange $ fromInteger n, t)
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

shallowAssign :: Location -> Expression () -> CodeWriter ()
shallowAssign (Just dst) src | dst /= src = tellProg [Assign dst src]
shallowAssign loc src = return ()

freshAliasInit :: Expression () -> CodeWriter (Expression ())
freshAliasInit e = do vexp <- freshAlias e
                      tellProg [Assign vexp e]
                      return vexp

shallowCopyWithRefSwap :: Expression () -> Expression () -> CodeWriter ()
shallowCopyWithRefSwap dst src 
  | dst /= src
  = case filter (hasReference . snd) $ flattenStructs $ typeof dst of
      [] -> tellProg [Assign dst src]
      arrs -> do temps <- sequence [freshAliasInit $ accF dst | (accF,t) <- arrs]
                 tellProg [Assign dst src]
                 tellProg [Assign (accF src) tmp | (tmp, (accF,t)) <- zip temps arrs]
  | otherwise = return ()

shallowCopyReferences :: Expression () -> Expression () -> CodeWriter ()
shallowCopyReferences dst src = tellProg [Assign (accF dst) (accF src) | (accF, t) <- flattenStructs $ typeof dst, hasReference t]

{- 
    This function implements double buffering of state for nonmutable seqential loops (forLoop and whileLoop).
    The intention is to generate the minimal amount of copying (especially deep array copies) while also avoiding
    frequent references to data that can not be allocated in registers.

    The state of the loop is implemented by two variables so that the loop body reads from one and writes to
    the other. At the end of the body, the contents of the second (write) variable is shallow copied to the 
    first (read) variable. In order to avoid inadvertent sharing of data referenced by pointers in the variables 
    (array buffers, for instance), the pointers in the state are swapped rather than just copied so that the end 
    up in the variable written to. Finally the read variable is shallow copied to the result location.

    There are some simplifying cases:
    - When the target lvalue loc is a variable, and thus cheap to access, it is reused as the read state variable
    - When the type of the state is scalar, so that assignment is atomic, only one state variable is used and it 
      is both read and written to in the loop body, eliminating the shallow copy in the loop body.

    The strategy implemented a compromise between different design constraints:
    - Avoid deep copying of arrays (that is what the double buffering is for)
    - Avoid shallow copying if possible
    - Avoid memory leaks of arrays and lvars
-}

mkDoubleBufferState :: Expression () -> VarId -> CodeWriter (Expression (), Expression ())
mkDoubleBufferState loc stvar
   = do stvar1 <- if isVarExpr loc || containsNativeArray (typeof loc) 
                     then return loc
                     else do vexp <- freshAlias loc
                             shallowCopyReferences vexp loc
                             return vexp
        stvar2 <- if isComposite $ typeof loc 
                     then do let vexp2 = mkVar (typeof loc) stvar 
                             declare vexp2
                             return vexp2
                     else return stvar1
        return (stvar1,stvar2)


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

withAlias :: VarId -> Expression () -> CodeWriter a -> CodeWriter a
withAlias v0 expr =
  local (\e -> e {alias = (v0,expr) : alias e})

isVariableOrLiteral :: ( Project (Core.Variable :|| Core.Type) dom
                       , Project (Core.Literal  :|| Core.Type) dom)
                    => AST (Decor info dom) a -> Bool
isVariableOrLiteral (prjF -> Just (C' (Core.Literal  _))) = True
isVariableOrLiteral (prjF -> Just (C' (Core.Variable _))) = True
isVariableOrLiteral _                                     = False

mkLength :: ( Project (Core.Literal  :|| Core.Type) dom
            , Project (Core.Variable :|| Core.Type) dom
            , Compile dom dom
            )
         => ASTF (Decor Info dom) a -> TypeRep a -> Core.Size a -> CodeWriter (Expression ())
mkLength a t sz 
  | isVariableOrLiteral a = compileExpr a
  | otherwise             = do
      lenvar    <- freshVar "len" t sz
      compileProg (Just lenvar) a
      return lenvar

mkBranch :: (Compile dom dom)
         => Location -> ASTF (Decor Info dom) Bool -> ASTF (Decor Info dom) a -> Maybe (ASTF (Decor Info dom) a) -> CodeWriter ()
mkBranch loc c th el = do
    ce <- compileExpr c
    (_, tb) <- confiscateBlock $ compileProg loc th
    (_, eb) <- if isJust el then confiscateBlock $ compileProg loc (fromJust el) else return (undefined, toBlock Empty)
    tellProg [Switch ce [(Pat (litB True), tb), (Pat (litB False), eb)]]


isComposite :: Type -> Bool
isComposite ArrayType{}   = True
isComposite NativeArray{} = True
isComposite StructType{}  = True
isComposite _             = False
