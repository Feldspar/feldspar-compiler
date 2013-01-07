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

import Language.Syntactic.Syntax hiding (result)
import Language.Syntactic.Traversal
import Language.Syntactic.Constraint
import Language.Syntactic.Constructs.Binding (VarId)

import Feldspar.Range
import Feldspar.Core.Types hiding (Type, ArrayType, BoolType, FloatType,
                                   ComplexType, IVarType, Signedness, Size)
import Feldspar.Core.Interpretation
import qualified Feldspar.Core.Types as Core
import qualified Feldspar.Core.Constructs.Binding as Core
import qualified Feldspar.Core.Constructs.Literal as Core

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.Representation (typeof, Place(..),
                                                    Type(..), Signedness(..),
                                                    Size(..), Variable(..),
                                                    VariableRole(..),
                                                    Expression(..),
                                                    Declaration(..))

import Feldspar.Compiler.Backend.C.Options (Options(..))
import Feldspar.Compiler.Backend.C.CodeGeneration (toC)

-- | Code generation monad
type CodeWriter = RWS Readers Writers States

data Readers = Readers { alias :: [(VarId, Expression ())] -- ^ variable aliasing
                       , sourceInfo :: SourceInfo -- ^ Surrounding source info
                       , backendOpts :: Options -- ^ Options for the backend.
                       }

initReader :: Options -> Readers
initReader opts = Readers [] "" opts

data Writers = Writers { block    :: Block  -- ^ collects code within one block
                       , def      :: [Ent]  -- ^ collects top level definitions
                       , decl     :: [Declaration ()]  -- ^ collects top level variable declarations
                       , epilogue :: [Prog] -- ^ collects postlude code (freeing memory, etc)
                       }

instance Monoid Writers
  where
    mempty      = Writers { block    = mempty
                          , def      = mempty
                          , decl     = mempty
                          , epilogue = mempty
                          }
    mappend a b = Writers { block    = mappend (block    a) (block    b)
                          , def      = mappend (def      a) (def      b)
                          , decl     = mappend (decl     a) (decl     b)
                          , epilogue = mappend (epilogue a) (epilogue b)
                          }

type Task = [Prog]

data States = States { fresh :: Integer -- ^ The first fresh variable id
                     }

initState :: States
initState = States 0

-- | Where to place the program result
type Location = Expression ()

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
    compileProgSym a info loc args
    return loc

compileProgDecor :: Compile dom dom
    => Location
    -> Decor Info dom a
    -> Args (AST (Decor Info dom)) a
    -> CodeWriter ()
compileProgDecor result (Decor info a) args = do
    let src = infoSource info
    aboveSrc <- asks sourceInfo
    unless (null src || src==aboveSrc) $ tellProg [Comment True src]
    local (\env -> env {sourceInfo = src}) $ compileProgSym a info result args

compileExprDecor :: Compile dom dom
    => Decor Info dom a
    -> Args (AST (Decor Info dom)) a
    -> CodeWriter (Expression ())
compileExprDecor (Decor info a) args = do
    let src = infoSource info
    aboveSrc <- asks sourceInfo
    unless (null src || src==aboveSrc) $ tellProg [Comment True src]
    local (\env -> env {sourceInfo = src}) $ compileExprSym a info args

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
        VarExpr (Variable{}) -> return e'
        _       -> do
            varId <- freshId
            let loc = varToExpr (Variable Value (typeof e') ('e' : show varId))
            declare loc
            assign loc e'
            return loc

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

compileNumType :: Core.Signedness a -> BitWidth n -> Type
compileNumType U N8      = (NumType Unsigned S8)
compileNumType S N8      = (NumType Signed S8)
compileNumType U N16     = (NumType Unsigned S16)
compileNumType S N16     = (NumType Signed S16)
compileNumType U N32     = (NumType Unsigned S32)
compileNumType S N32     = (NumType Signed S32)
compileNumType U N64     = (NumType Unsigned S64)
compileNumType S N64     = (NumType Signed S64)
compileNumType U NNative = (NumType Unsigned S32)  -- TODO
compileNumType S NNative = (NumType Signed S32)  -- TODO

compileTypeRep :: TypeRep a -> Core.Size a -> Type
compileTypeRep UnitType _                = VoidType
compileTypeRep Core.BoolType _           = BoolType
compileTypeRep (IntType s n) _           = compileNumType s n
compileTypeRep Core.FloatType _          = FloatType
compileTypeRep (Core.ComplexType t) _    = ComplexType (compileTypeRep t (defaultSize t))
compileTypeRep (Tup2Type a b) (sa,sb)          = StructType
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        ]
compileTypeRep (Tup3Type a b c) (sa,sb,sc)        = StructType
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        , ("member3", compileTypeRep c sc)
        ]
compileTypeRep (Tup4Type a b c d) (sa,sb,sc,sd)      = StructType
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        , ("member3", compileTypeRep c sc)
        , ("member4", compileTypeRep d sd)
        ]
compileTypeRep (Tup5Type a b c d e) (sa,sb,sc,sd,se)    = StructType
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        , ("member3", compileTypeRep c sc)
        , ("member4", compileTypeRep d sd)
        , ("member5", compileTypeRep e se)
        ]
compileTypeRep (Tup6Type a b c d e f) (sa,sb,sc,sd,se,sf)  = StructType
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        , ("member3", compileTypeRep c sc)
        , ("member4", compileTypeRep d sd)
        , ("member5", compileTypeRep e se)
        , ("member6", compileTypeRep f sf)
        ]
compileTypeRep (Tup7Type a b c d e f g) (sa,sb,sc,sd,se,sf,sg) = StructType
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
compileTypeRep (Core.IVarType a) _      = IVarType $ compileTypeRep a $ defaultSize a
compileTypeRep (FunType _ b) sz         = compileTypeRep b sz
compileTypeRep (FValType a) sz          = IVarType $ compileTypeRep a sz
compileTypeRep typ _                    = error $ "compileTypeRep: missing " ++ show typ  -- TODO

mkVarName :: VarId -> String
mkVarName v = 'v' : show v

-- | Construct a variable.
mkVar :: Type -> VarId -> Expression ()
mkVar t = varToExpr . Variable Value t . mkVarName

-- | Construct a pointer.
mkRef :: Type -> VarId -> Expression ()
mkRef t = varToExpr . Variable Pointer t . mkVarName

mkVariable :: Type -> VarId -> Variable ()
mkVariable t = Variable Value t . mkVarName

freshId :: CodeWriter Integer
freshId = do
  s <- get
  let v = fresh s
  put (s {fresh = v + 1})
  return v

freshVar :: String -> TypeRep a -> Core.Size a -> CodeWriter (Expression ()) -- TODO take just info instead of TypeRep and Size?
freshVar base t size = do
  v <- freshId
  let var = varToExpr . Variable Value (compileTypeRep t size) $ base ++ show v
  declare var
  return var

declare :: Expression () -> CodeWriter ()
declare (VarExpr v@(Variable{})) = tellDecl [Declaration v Nothing]
declare expr      = error $ "declare: cannot declare expression: " ++ show expr

initialize :: Expression () -> Expression () -> CodeWriter ()
initialize (VarExpr v@(Variable{})) e = tellDecl [Declaration v (Just e)]
initialize expr      _ = error $ "initialize: cannot declare expression: " ++ show expr

tellDef :: [Ent] -> CodeWriter ()
tellDef es = tell $ mempty {def = es}

tellProg :: [Prog] -> CodeWriter ()
tellProg [Block [] ps] = tell $ mempty {block = Bl [] $ Seq [ps]}
tellProg ps = tell $ mempty {block = Bl [] $ Seq ps}

tellDecl :: [Declaration ()] -> CodeWriter ()
tellDecl ds = do rs <- ask
                 let frees = freeArrays ds ++ freeIVars ds
                     defs = getTypes (backendOpts rs) ds
                     code | True = mempty {decl=ds, epilogue = frees, def = defs}
                          | otherwise = mempty {block = Bl ds $ Seq [], epilogue = frees, def = defs}
                 tell code

getTypes :: Options -> [Declaration ()] -> [Ent]
getTypes opts defs = mkDef comps
  where
    comps = filter (isComposite) $ map (typeof . dVar) defs
    -- There are other composite types that are not flagged as such by this
    -- version of isComposite, so keep it private.
    isComposite :: Type -> Bool
    isComposite (StructType {}) = True
    isComposite e = isArray e
    -- TODO: There is nothing in the frontend for naming structures in
    -- Prog, that is done in CodeGeneration by toC. We should combine
    -- Prog and Program into a single format and untangle naming from
    -- prettyprinting.
    mkDef [] = []
    mkDef (typ:typs)
      | s@(StructType members) <- typ
      = mkDef (map snd members) ++ (StructD (toC opts Declaration_pl s) members):mkDef typs
      | (ArrayType _ typ2) <- typ = mkDef (typ2:typs)
      | otherwise = mkDef typs

assign :: Location -> Expression () -> CodeWriter ()
assign lhs rhs = tellProg [copyProg lhs [rhs]]

-- | Like 'listen', but also prevents the program from being written in the
-- monad.
confiscateBlock :: CodeWriter a -> CodeWriter (a, Block)
confiscateBlock m
    = liftM (second block)
    $ censor (\rec -> rec {block = mempty})
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
      compileProg lenvar a
      return lenvar

