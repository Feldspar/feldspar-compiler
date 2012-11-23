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
import Feldspar.Core.Types hiding (Type)
import Feldspar.Core.Interpretation
import qualified Feldspar.Core.Types as Core
import qualified Feldspar.Core.Constructs.Binding as Core
import qualified Feldspar.Core.Constructs.Literal as Core

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.Representation (typeof)

-- | Code generation monad
type CodeWriter = RWS Readers Writers States

data Readers = Readers { alias :: [(VarId, Expr)] -- ^ variable aliasing
                       , sourceInfo :: SourceInfo -- ^ Surrounding source info
                       }

initReader :: Readers
initReader = Readers [] ""

data Writers = Writers { block :: Block -- ^ collects code within one block
                       , def   :: [Ent] -- ^ collects top level definitions
                       }

instance Monoid Writers
  where
    mempty      = Writers { block    = mempty
                          , def      = mempty
                          }
    mappend a b = Writers { block    = mappend (block a) (block b)
                          , def      = mappend (def   a) (def   b)
                          }

type Task = [Prog]

data States = States { fresh :: Integer -- ^ The first fresh variable id
                     }

initState :: States
initState = States 0

-- | Where to place the program result
type Location = Expr

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
        -> CodeWriter Expr
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
    -> CodeWriter Expr
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
    unless (null src || src==aboveSrc) $ tellProg [BComment src]
    local (\env -> env {sourceInfo = src}) $ compileProgSym a info result args

compileExprDecor :: Compile dom dom
    => Decor Info dom a
    -> Args (AST (Decor Info dom)) a
    -> CodeWriter Expr
compileExprDecor (Decor info a) args = do
    let src = infoSource info
    aboveSrc <- asks sourceInfo
    unless (null src || src==aboveSrc) $ tellProg [BComment src]
    local (\env -> env {sourceInfo = src}) $ compileExprSym a info args

compileProg :: Compile dom dom =>
    Location -> ASTF (Decor Info dom) a -> CodeWriter ()
compileProg result = simpleMatch (compileProgDecor result)

compileExpr :: Compile dom dom => ASTF (Decor Info dom) a -> CodeWriter Expr
compileExpr = simpleMatch compileExprDecor

-- Compile an expression and make sure that the result is stored in a variable
compileExprVar :: Compile dom dom => ASTF (Decor Info dom) a -> CodeWriter Expr
compileExprVar e = do
    e' <- compileExpr e
    case e' of
        Var _ _ -> return e'
        Ptr _ _ -> return e'
        _       -> do
            varId <- freshId
            let loc = Var (typeof e') ('e' : show varId)
            declare loc
            assign loc e'
            return loc

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

compileNumType :: Signedness a -> BitWidth n -> Type
compileNumType U N8      = U8
compileNumType S N8      = I8
compileNumType U N16     = U16
compileNumType S N16     = I16
compileNumType U N32     = U32
compileNumType S N32     = I32
compileNumType U N64     = U64
compileNumType S N64     = I64
compileNumType U NNative = U32  -- TODO
compileNumType S NNative = I32  -- TODO

compileTypeRep :: TypeRep a -> Size a -> Type
compileTypeRep UnitType _                = Void
compileTypeRep BoolType _                = Boolean
compileTypeRep (IntType s n) _           = compileNumType s n
compileTypeRep FloatType _               = Floating
compileTypeRep (ComplexType t) _         = Complex (compileTypeRep t (defaultSize t))
compileTypeRep (Tup2Type a b) (sa,sb)          = Struct
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        ]
compileTypeRep (Tup3Type a b c) (sa,sb,sc)        = Struct
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        , ("member3", compileTypeRep c sc)
        ]
compileTypeRep (Tup4Type a b c d) (sa,sb,sc,sd)      = Struct
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        , ("member3", compileTypeRep c sc)
        , ("member4", compileTypeRep d sd)
        ]
compileTypeRep (Tup5Type a b c d e) (sa,sb,sc,sd,se)    = Struct
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        , ("member3", compileTypeRep c sc)
        , ("member4", compileTypeRep d sd)
        , ("member5", compileTypeRep e se)
        ]
compileTypeRep (Tup6Type a b c d e f) (sa,sb,sc,sd,se,sf)  = Struct
        [ ("member1", compileTypeRep a sa)
        , ("member2", compileTypeRep b sb)
        , ("member3", compileTypeRep c sc)
        , ("member4", compileTypeRep d sd)
        , ("member5", compileTypeRep e se)
        , ("member6", compileTypeRep f sf)
        ]
compileTypeRep (Tup7Type a b c d e f g) (sa,sb,sc,sd,se,sf,sg) = Struct
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
compileTypeRep (ArrayType a) (rs :> es) = SizedArray rs $ compileTypeRep a es
compileTypeRep (MArrType a) (rs :> es)  = SizedArray rs $ compileTypeRep a es
compileTypeRep (ParType a) _            = compileTypeRep a (defaultSize a)
compileTypeRep (IVarType a) _           = IVar $ compileTypeRep a $ defaultSize a
compileTypeRep (FunType _ b) sz         = compileTypeRep b sz
compileTypeRep (FValType a) sz          = IVar $ compileTypeRep a sz
compileTypeRep typ _                    = error $ "compileTypeRep: missing " ++ show typ  -- TODO

mkVarName :: VarId -> String
mkVarName v = 'v' : show v

mkVar :: Type -> VarId -> Expr
mkVar t = Var t . mkVarName

mkVariable :: Type -> VarId -> Var
mkVariable t = Variable t . mkVarName

freshId :: CodeWriter Integer
freshId = do
  s <- get
  let v = fresh s
  put (s {fresh = v + 1})
  return v

freshVar :: String -> TypeRep a -> Size a -> CodeWriter Expr -- TODO take just info instead of TypeRep and Size?
freshVar base t size = do
  v <- freshId
  let var =Var (compileTypeRep t size) $ base ++ show v
  declare var
  return var

declare :: Expr -> CodeWriter ()
declare (Var t n) = tellDecl [Def t n]
declare (Ptr t n) = tellDecl [Def t n]
declare expr      = error $ "declare: cannot declare expression: " ++ show expr

tellDef :: [Ent] -> CodeWriter ()
tellDef es = tell $ mempty {def = es}

tellProg :: [Prog] -> CodeWriter ()
tellProg ps = tell $ mempty {block = Bl [] $ Seq ps}

tellDecl :: [Def] -> CodeWriter ()
tellDecl ds = tell $ mempty {block = Bl ds $ Seq []}

assign :: Location -> Expr -> CodeWriter ()
assign lhs rhs = tellProg [copyProg lhs rhs]

-- | Like 'listen', but also prevents the program from being written in the
-- monad.
confiscateBlock :: CodeWriter a -> CodeWriter (a, Block)
confiscateBlock m
    = liftM (second block)
    $ censor (\rec -> rec {block = mempty})
    $ listen m

withAlias :: VarId -> Expr -> CodeWriter a -> CodeWriter a
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
         => ASTF (Decor Info dom) a -> TypeRep a -> Size a -> CodeWriter Expr
mkLength a t sz 
  | isVariableOrLiteral a = compileExpr a
  | otherwise             = do
      lenvar    <- freshVar "len" t sz
      compileProg lenvar a
      return lenvar

