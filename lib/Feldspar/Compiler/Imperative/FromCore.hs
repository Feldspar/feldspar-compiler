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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module that translates from the UntypedFeld program from the middleend to
--   the module format in the backend.
module Feldspar.Compiler.Imperative.FromCore (
    fromCore
  , getCore'
  )
  where

import Data.Char (toLower)
import Data.List (nub, partition)
import Data.Maybe (isJust, fromJust)

import Control.Monad.RWS
import Control.Applicative

import Feldspar.Core.Types
import Feldspar.Core.UntypedRepresentation
         ( Term(..), Lit(..), collectLetBinders, collectBinders
         , UntypedFeldF(App, LetFun), Fork(..), typeof
         )
import qualified Feldspar.Core.UntypedRepresentation as Ut
import Feldspar.Core.Middleend.FromTyped
import Feldspar.Compiler.Backend.C.Platforms (extend, c99)
import Feldspar.Core.Constructs (SyntacticFeld)
import Feldspar.Core.Frontend (reifyFeld)

import qualified Feldspar.Compiler.Imperative.Representation as Rep (Variable(..), Type(..), ScalarType(..))
import Feldspar.Compiler.Imperative.Representation
         ( ActualParameter(..), Block(..), Declaration(..), Entity(..)
         , Expression(..), Module(..), Program(..), Pattern(..), ParType(..)
         , Constant(..), typeof, fv
         )
import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation
import Feldspar.Compiler.Backend.C.Options (Options(..))

{-

Fast returns
------------

Fast returns really means single return value and that value fits in a
register--thus they are platform dependent. This is why we have to
compileTypeRep since that can make configuration specific choices
about data layout.

The user is free to ask for fast returns, but we might not be able to comply.
For those cases we flip the option before generating code.

-}

-- | Get the generated core for a program with a specified output name.
fromCore :: SyntacticFeld a => Options -> String -> a -> Module ()
fromCore opt funname prog = Module defs
  where
    (outParam,results) = evalRWS (compileProgTop opt' ast) (initReader opt) initState
    opt' | useNativeReturns opt
         , not $ canFastReturn $ compileTypeRep opt (typeof ast)
         = opt { useNativeReturns = False } -- Note [Fast returns]
         | otherwise = opt
    fastRet    = useNativeReturns opt'
    ast        = untype (frontendOpts opt) $ reifyFeld (frontendOpts opt) N32 prog
    decls      = decl results
    ins        = params results
    post       = epilogue results ++ returns
    Block ds p = block results
    outDecl    = Declaration outParam Nothing
    paramTypes = getTypes $ outDecl:map (`Declaration` Nothing) ins
    defs       = nub (def results ++ paramTypes) ++ topProc
    (outs, ds', returns)
     | fastRet   = ( Right outParam,  outDecl:ds ++ decls
                   , [call "return" [ValueParameter $ varToExpr outParam]])
     | otherwise = ( Left [outParam],         ds ++ decls, [])
    topProc    = [Proc funname ins outs $ Just (Block ds' (Sequence (p:post)))]

-- | Get the generated core for a program.
getCore' :: SyntacticFeld a => Options -> a -> Module ()
getCore' opts = fromCore opts "test"

compileProgTop :: Options -> Ut.UntypedFeld -> CodeWriter (Rep.Variable ())
compileProgTop opt (In (Ut.Lambda (Ut.Var v ta) body)) = do
  let typ = compileTypeRep opt ta
      (arg,arge) | Rep.StructType{} <- typ = (mkPointer typ v, Deref $ varToExpr arg)
                 | otherwise               = (mkVariable typ v, varToExpr arg)
  tell $ mempty {params=[arg]}
  withAlias v arge $
     compileProgTop opt body
compileProgTop opt (In (Ut.App Ut.Let _ [In (Ut.Literal l), In (Ut.Lambda (Ut.Var v _) body)]))
  | representableType l
  = do tellDef [ValueDef var c]
       withAlias v (varToExpr var) $
         compileProgTop opt body
  where
    var = mkVariable (typeof c) v -- Note [Precise size information]
    c   = literalConst l
compileProgTop opt a = do
  let outType' = compileTypeRep opt (typeof a)
      (outType, outLoc)
       | useNativeReturns opt = (outType',             varToExpr outParam)
       | otherwise            = (Rep.Pointer outType', Deref $ varToExpr outParam)
      outParam   = Rep.Variable outType "out"
  compileProg (cenv0 opt) (Just outLoc) a
  return outParam

{-

Precise size information
------------------------

Tight size bounds for a given literal is easy to compute. Precise
bounds are particularly important for array literals since they are
often copied in the deepCopy function near the CodeGen in the
backend. Deepcopy will appear to hang when generating the copy code
for insanely large array literals, so don't do that.

-}

data CompileEnv = CEnv
    { opts :: Options
    , inTask :: Bool
    }

-- | Initial environment for compile.
cenv0 :: Options -> CompileEnv
cenv0 opts = CEnv opts False

-- | Compiles code and assigns the expression to the given location.
compileExprLoc :: CompileEnv -> Location  -> Ut.UntypedFeld  -> CodeWriter ()
compileExprLoc env loc e = do
    expr <- compileExpr env e
    assign loc expr

-- | Compiles code into a fresh variable.
compileProgFresh :: CompileEnv -> Ut.UntypedFeld -> CodeWriter (Expression ())
compileProgFresh env e = do
    loc <- freshVar (opts env) "e" (typeof e)
    compileProg env (Just loc) e
    return loc

-- | Compile an expression and make sure that the result is stored in a variable
compileExprVar :: CompileEnv -> Ut.UntypedFeld -> CodeWriter (Expression ())
compileExprVar env e = do
    e' <- compileExpr env e
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

-- | Compile a function bound by a LetFun.
compileFunction :: CompileEnv -> Expression () -> (String, Fork, Ut.UntypedFeld)
                -> CodeWriter ()
compileFunction env loc (coreName, kind, e) | (bs, e') <- collectBinders e = do
  es' <- mapM (compileExpr env) (map (In . Ut.Variable) bs)
  let args = nub $ map exprToVar es' ++ fv loc
  -- Task core:
  ((_, ws), Block ds bl)  <- confiscateBigBlock $
    case kind of
      Future -> do
        p' <- compileExprVar env {inTask = True } e'
        tellProg [iVarPut loc p']
      Par -> compileProg env {inTask = True} (Just loc) e'
      Loop | (ix:_) <- es' -> compileProg env (Just $ ArrayElem loc ix) e'
      None -> compileProg env (Just loc) e'
  tellDef [Proc coreName args (Left []) $ Just $ Block (decl ws ++ ds) bl]
  -- Task:
  let taskName = "task" ++ drop 9 coreName
      runTask  = Just $ toBlock $ run coreName args
      outs     = [mkNamedRef "params" Rep.VoidType (-1)]
  case kind of
   _ | kind `elem` [None, Loop] -> return ()
   _    -> tellDef [Proc taskName [] (Left outs) runTask]

-- | Create a variable of the right type for storing a length.
mkLength :: CompileEnv -> Ut.UntypedFeld -> Ut.Type -> CodeWriter (Expression ())
mkLength env a t
  | isVariableOrLiteral a = compileExpr env a
  | otherwise             = do
      lenvar <- freshVar (opts env) "len" t
      compileProg env (Just lenvar) a
      return lenvar

mkBranch :: CompileEnv -> Location -> Ut.UntypedFeld -> Ut.UntypedFeld
         -> Maybe Ut.UntypedFeld -> CodeWriter ()
mkBranch env loc c th el = do
    ce <- compileExpr env c
    (_, tb) <- confiscateBlock $ compileProg env loc th
    (_, eb) <- if isJust el
                  then confiscateBlock $ compileProg env loc (fromJust el)
                  else return (undefined, toBlock Empty)
    tellProg [Switch ce [(Pat (litB True), tb), (Pat (litB False), eb)]]

compileProg :: CompileEnv -> Location -> Ut.UntypedFeld -> CodeWriter ()
-- Array
compileProg env loc (In (App Ut.Parallel _ [len, In (Ut.Lambda (Ut.Var v ta) ixf)])) = do
   let ix = mkVar (compileTypeRep (opts env) ta) v
   len' <- mkLength env len ta
   (_, b) <- confiscateBlock $ compileProg env (ArrayElem <$> loc <*> pure ix) ixf
   tellProg [initArray loc len']
   tellProg [for Parallel (lName ix) len' (litI32 1) b]
compileProg env loc (In (App Ut.Sequential _ [len, init', In (Ut.Lambda (Ut.Var v tix) ixf1)]))
   | In (Ut.Lambda (Ut.Var s tst) l) <- ixf1
   , (bs, In (Ut.App Ut.Tup2 _ [In (Ut.Variable t1), In (Ut.Variable t2)])) <- collectLetBinders l
   , not $ null bs
   , (e, step) <- last bs
   , t1 == e
   , t2 == e
   = do
        blocks <- mapM (confiscateBlock . compileBind env) (init bs)
        let (dss, lets) = unzip $ map (\(_, Block ds (Sequence body)) -> (ds, body)) blocks
        let ix = mkVar (compileTypeRep (opts env) tix) v
        len' <- mkLength env len tix
        st1 <- freshVar (opts env) "st" tst
        let st = mkRef (compileTypeRep (opts env) tst) s
            st_val = Deref st
        declareAlias st
        (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s st_val $ compileProg env (ArrayElem <$> loc <*> pure ix) step
        withAlias s st_val $ compileProg env (Just st1) init'
        tellProg [ Assign (Just st) (AddrOf st1)
                 , initArray loc len']
        tellProg [toProg $ Block (concat dss ++ ds) $
                  for Sequential (lName ix) len' (litI32 1) $
                               toBlock $ Sequence (concat lets ++ body ++ maybe [] (\arr -> [Assign (Just st) $ AddrOf (ArrayElem arr ix)]) loc)]
compileProg env loc (In (App Ut.Sequential _ [len, st, In (Ut.Lambda (Ut.Var v t) (In (Ut.Lambda (Ut.Var s _) step)))]))
  = do
       let tr' = typeof step
       let ix = mkVar (compileTypeRep (opts env) t) v
       len' <- mkLength env len t
       tmp  <- freshVar (opts env) "seq" tr'
       (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s (StructField tmp "member2") $ compileProg env (Just tmp) step
       tellProg [initArray loc len']
       compileProg env (Just $ StructField tmp "member2") st
       tellProg [toProg $ Block ds $
                 for Sequential (lName ix) len' (litI32 1) $ toBlock $
                   Sequence $ body ++
                     [copyProg (ArrayElem <$> loc <*> pure ix) [StructField tmp "member1"]
                     ]]
compileProg env loc (In (App Ut.Append _ [a, b])) = do
   a' <- compileExpr env a
   b' <- compileExpr env b
   tellProg [copyProg loc [a', b']]
compileProg env loc (In (App Ut.SetIx _ [arr, i, a])) = do
   compileProg env loc arr
   i' <- compileExpr env i
   compileProg env (ArrayElem <$> loc <*> pure i') a
compileProg env (Just loc) (In (App Ut.GetIx _ [arr, i])) = do
   a' <- compileExpr env arr
   i' <- compileExpr env i
   let el = ArrayElem a' i'
   tellProg $ if isArray $ typeof el
                then [Assign (Just loc) el]
                else [copyProg (Just loc) [el]]
compileProg env loc (In (App Ut.SetLength _ [len, arr])) = do
   len' <- compileExpr env len
   compileProg env loc arr
   tellProg [setLength loc len']
-- Binding
compileProg _   _   e@(In Ut.Lambda{})
  = error ("Can only compile top-level lambda: " ++ show e)
compileProg env loc (In (Ut.App Ut.Let _ [a, In (Ut.Lambda (Ut.Var v ta) body)])) = do
   e <- compileLet env a ta v
   withAlias v e $ compileProg env loc body
-- Bits
-- Complex
-- Condition
compileProg env loc (In (App Ut.Condition _ [cond, tHEN, eLSE])) =
   mkBranch env loc cond tHEN $ Just eLSE
compileProg env loc (In (App Ut.ConditionM _ [cond, tHEN, eLSE])) =
   mkBranch env loc cond tHEN $ Just eLSE
-- Conversion
-- Elements
compileProg env loc (In (App Ut.EMaterialize t [len, arr])) = do
   len' <- mkLength env len t
   tellProg [initArray loc len']
   compileProg env loc arr
compileProg env (Just loc) (In (App Ut.EWrite _ [ix, e])) = do
   dst <- compileExpr env ix
   compileProg env (Just $ ArrayElem loc dst) e
compileProg _   _   (In (App Ut.ESkip _ _)) = return ()
compileProg env loc (In (App Ut.EPar _ [p1, p2])) = do
   (_, Block ds1 b1) <- confiscateBlock $ compileProg env loc p1
   (_, Block ds2 b2) <- confiscateBlock $ compileProg env loc p2
   tellProg [toProg $ Block (ds1 ++ ds2) (Sequence [b1,b2])]
compileProg env loc (In (App Ut.EparFor _ [len, In (Ut.Lambda (Ut.Var v ta) ixf)])) = do
   let ix = mkVar (compileTypeRep (opts env) ta) v
   len' <- mkLength env len ta
   (_, ixf') <- confiscateBlock $ compileProg env loc ixf
   tellProg [for Parallel (lName ix) len' (litI32 1) ixf']
-- Error
compileProg _   _   (In (App Ut.Undefined _ _)) = return ()
compileProg env loc (In (App (Ut.Assert msg) _ [cond, a])) = do
   compileAssert env cond msg
   compileProg env loc a
-- Future
compileProg _  _ e@(In (App Ut.MkFuture _ _))
  = error ("Unexpected MkFuture:" ++ show e)
compileProg env (Just loc) (In (LetFun f e)) = do
   compileFunction env loc f
   compileProg env (Just loc) e
compileProg env loc (In (App Ut.Await _ [a])) = do
   fut <- compileExprVar env a
   tellProg [iVarGet (inTask env) l fut | Just l <- [loc]]
-- Literal
compileProg env loc (In (Ut.Literal a)) = case loc of
     Just l -> literalLoc env l a
     Nothing -> return ()
-- Logic
-- Loop
compileProg env (Just loc) (In (App Ut.ForLoop _ [len, init', In (Ut.Lambda (Ut.Var ix ta) (In (Ut.Lambda (Ut.Var st stt) ixf)))]))
  = do
      let ix' = mkVar (compileTypeRep (opts env) ta) ix
      len' <- mkLength env len ta
      (lstate, stvar) <- mkDoubleBufferState loc st
      compileProg env (Just lstate) init'
      (_, Block ds body) <- withAlias st lstate $ confiscateBlock
                          $ compileProg env (Just stvar) ixf
                          >> shallowCopyWithRefSwap lstate stvar
      tellProg [toProg $ Block ds (for Sequential (lName ix') len' (litI32 1) (toBlock body))]
      shallowAssign (Just loc) lstate
compileProg env (Just loc) (In (App Ut.WhileLoop t [init', In (Ut.Lambda (Ut.Var cv ct) cond), In (Ut.Lambda (Ut.Var bv bt) body)])) = do
    let condv = mkVar (compileTypeRep (opts env) (typeof cond)) cv
    (lstate,stvar) <- mkDoubleBufferState loc bv
    compileProg env (Just lstate) init'
    (_, cond') <- confiscateBlock $ withAlias cv lstate $ compileProg env (Just condv) cond
    (_, body') <- withAlias bv lstate $ confiscateBlock $ compileProg env (Just stvar) body >> shallowCopyWithRefSwap lstate stvar
    declare condv
    tellProg [while cond' condv body']
    shallowAssign (Just loc) lstate
-- LoopM
compileProg env loc (In (App Ut.While _ [In (Ut.Lambda _ cond), step])) = do
   condv <- freshVar (opts env) "cond" (typeof cond)
   (_, cond') <- confiscateBlock $ compileProg env (Just condv) cond
   (_, step') <- confiscateBlock $ compileProg env loc step
   tellProg [while cond' condv step']
compileProg env loc (In (App Ut.For _ [len, In (Ut.Lambda (Ut.Var v ta) ixf)])) = do
   let ix = mkVar (compileTypeRep (opts env) ta) v
   len' <- mkLength env len ta
   (_, Block ds body) <- confiscateBlock $ compileProg env loc ixf
   tellProg [toProg $ Block ds (for Sequential (lName ix) len' (litI32 1) (toBlock body))]
-- Mutable
compileProg env loc (In (App Ut.Run _ [ma])) = compileProg env loc ma
compileProg env loc (In (App Ut.Return t [a]))
  | Ut.MutType Ut.UnitType <- t = return ()
  | Ut.ParType Ut.UnitType <- t = return ()
  | otherwise = compileProg env loc a
compileProg env loc (In (App Ut.Bind _ [ma, In (Ut.Lambda (Ut.Var v ta) body)]))
  | (In (App Ut.ParNew _ _)) <- ma = do
   let var = mkVar (compileTypeRep (opts env) ta) v
   declare var
   tellProg [iVarInit (AddrOf var)]
   compileProg env loc body
  | otherwise = do
   let var  = mkVar (compileTypeRep (opts env) ta) v
   declare var
   compileProg env (Just var) ma
   compileProg env loc body
compileProg env loc (In (App Ut.Then _ [ma, mb])) = do
   compileProg env Nothing ma
   compileProg env loc mb
compileProg env loc (In (App Ut.When _ [c, action])) =
   mkBranch env loc c action Nothing
-- MutableArray
compileProg env loc (In (App Ut.NewArr _ [len, a])) = do
   nId <- freshId
   let ix = varToExpr $ mkNamedVar "i" (Rep.MachineVector 1 (Rep.NumType Ut.Unsigned Ut.S32)) nId
   a' <- compileExpr env a
   l  <- compileExpr env len
   tellProg [initArray loc l]
   tellProg [for Sequential "i" l (litI32 1) $ toBlock (Sequence [copyProg (ArrayElem <$> loc <*> pure ix) [a']])]
compileProg env loc (In (App Ut.NewArr_ _ [len])) = do
   l <- compileExpr env len
   tellProg [initArray loc l]
compileProg env loc (In (App Ut.GetArr _ [arr, i])) = do
   arr' <- compileExpr env arr
   i'   <- compileExpr env i
   assign loc (ArrayElem arr' i')
compileProg env _ (In (App Ut.SetArr _ [arr, i, a])) = do
   arr' <- compileExpr env arr
   i'   <- compileExpr env i
   a'   <- compileExpr env a
   assign (Just $ ArrayElem arr' i') a'
-- MutableReference
compileProg env loc (In (App Ut.NewRef _ [a])) = compileProg env loc a
compileProg env loc (In (App Ut.GetRef _ [r])) = compileProg env loc r
compileProg env _ (In (App Ut.SetRef _ [r, a])) = do
   var  <- compileExpr env r
   compileProg env (Just var) a
compileProg env _ (In (App Ut.ModRef _ [r, In (Ut.Lambda (Ut.Var v _) body)])) = do
   var <- compileExpr env r
   withAlias v var $ compileProg env (Just var) body
       -- Since the modifier function is pure it is safe to alias
       -- v with var here
-- MutableToPure
compileProg env (Just loc) (In (App Ut.RunMutableArray _ [marr]))
 | (In (App Ut.Bind _ [In (App Ut.NewArr_ _ [l]), In (Ut.Lambda (Ut.Var v _) body)])) <- marr
 , (In (App Ut.Return _ [In (Ut.Variable (Ut.Var r _))])) <- chaseBind body
 , v == r
 = do
     len <- compileExpr env l
     tellProg [setLength (Just loc) len]
     withAlias v loc $ compileProg env (Just loc) body
compileProg env loc (In (App Ut.RunMutableArray _ [marr])) = compileProg env loc marr
compileProg env loc (In (App Ut.WithArray _ [marr@(In Ut.Variable{}), In (Ut.Lambda (Ut.Var v _) body)])) = do
    e <- compileExpr env marr
    withAlias v e $ do
      b <- compileExpr env body
      tellProg [copyProg loc [b]]
compileProg env loc (In (App Ut.WithArray _ [marr, In (Ut.Lambda (Ut.Var v ta) body)])) = do
    let var = mkVar (compileTypeRep (opts env) ta) v
    declare var
    compileProg env (Just var) marr
    e <- compileExpr env body
    tellProg [copyProg loc [e]]
-- Noinline
compileProg _ (Just _) (In (App Ut.NoInline _ [e]))
  = error ("Unexpected NoInline:" ++ show e)
-- Par
compileProg env loc (In (App Ut.ParRun _ [p])) = compileProg env loc p
compileProg _   _   (In (App Ut.ParNew _ _)) = return ()
compileProg env loc (In (App Ut.ParGet _ [r])) = do
    iv <- compileExpr env r
    tellProg [iVarGet (inTask env) l iv | Just l <- [loc]]
compileProg env _ (In (App Ut.ParPut _ [r, a])) = do
    iv  <- compileExpr env r
    val <- compileExpr env a
    i   <- freshId
    let var = varToExpr $ mkNamedVar "msg" (typeof val) i
    declare var
    assign (Just var) val
    tellProg [iVarPut iv var]
compileProg _ _ (In (App Ut.ParFork _ [e]))
  = error ("Unexpected ParFork:" ++ show e)
compileProg _ _ (In (App Ut.ParYield _ _)) = return ()
-- SizeProp
compileProg env loc (In (App Ut.PropSize _ [e])) = compileProg env loc e
-- SourceInfo
compileProg env loc (In (App (Ut.SourceInfo info) _ [a])) = do
    tellProg [Comment True info]
    compileProg env loc a
-- Switch
compileProg env loc (In (App Ut.Switch _ [tree@(In (App Ut.Condition _ [In (App Ut.Equal _ [_, s]), _, _]))])) = do
    scrutinee <- compileExpr env s
    alts      <- chaseTree env loc s tree
    tellProg [Switch{..}]
compileProg env loc (In (App Ut.Switch _ [tree])) = compileProg env loc tree
-- Tuple
compileProg env loc (In (App Ut.Tup2 _ [m1, m2])) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
compileProg env loc (In (App Ut.Tup3 _ [m1, m2, m3])) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
compileProg env loc (In (App Ut.Tup4 _ [m1, m2, m3, m4])) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
compileProg env loc (In (App Ut.Tup5 _ [m1, m2, m3, m4, m5])) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
    compileProg env (StructField <$> loc <*> pure "member5") m5
compileProg env loc (In (App Ut.Tup6 _ [m1, m2, m3, m4, m5, m6])) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
    compileProg env (StructField <$> loc <*> pure "member5") m5
    compileProg env (StructField <$> loc <*> pure "member6") m6
compileProg env loc (In (App Ut.Tup7 _ [m1, m2, m3, m4, m5, m6, m7])) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
    compileProg env (StructField <$> loc <*> pure "member5") m5
    compileProg env (StructField <$> loc <*> pure "member6") m6
    compileProg env (StructField <$> loc <*> pure "member7") m7
compileProg env loc (In (App Ut.Tup8 _ [m1, m2, m3, m4, m5, m6, m7, m8])) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
    compileProg env (StructField <$> loc <*> pure "member5") m5
    compileProg env (StructField <$> loc <*> pure "member6") m6
    compileProg env (StructField <$> loc <*> pure "member7") m7
    compileProg env (StructField <$> loc <*> pure "member8") m8
compileProg env loc (In (App Ut.Tup9 _ [m1, m2, m3, m4, m5, m6, m7, m8, m9])) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
    compileProg env (StructField <$> loc <*> pure "member5") m5
    compileProg env (StructField <$> loc <*> pure "member6") m6
    compileProg env (StructField <$> loc <*> pure "member7") m7
    compileProg env (StructField <$> loc <*> pure "member8") m8
    compileProg env (StructField <$> loc <*> pure "member9") m9
compileProg env loc (In (App Ut.Tup10 _ [m1, m2, m3, m4, m5, m6, m7, m8, m9, m10])) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
    compileProg env (StructField <$> loc <*> pure "member5") m5
    compileProg env (StructField <$> loc <*> pure "member6") m6
    compileProg env (StructField <$> loc <*> pure "member7") m7
    compileProg env (StructField <$> loc <*> pure "member8") m8
    compileProg env (StructField <$> loc <*> pure "member9") m9
    compileProg env (StructField <$> loc <*> pure "member10") m10
compileProg env loc (In (App Ut.Tup11 _ [m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11])) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
    compileProg env (StructField <$> loc <*> pure "member5") m5
    compileProg env (StructField <$> loc <*> pure "member6") m6
    compileProg env (StructField <$> loc <*> pure "member7") m7
    compileProg env (StructField <$> loc <*> pure "member8") m8
    compileProg env (StructField <$> loc <*> pure "member9") m9
    compileProg env (StructField <$> loc <*> pure "member10") m10
    compileProg env (StructField <$> loc <*> pure "member11") m11
compileProg env loc (In (App Ut.Tup12 _ [m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12])) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
    compileProg env (StructField <$> loc <*> pure "member5") m5
    compileProg env (StructField <$> loc <*> pure "member6") m6
    compileProg env (StructField <$> loc <*> pure "member7") m7
    compileProg env (StructField <$> loc <*> pure "member8") m8
    compileProg env (StructField <$> loc <*> pure "member9") m9
    compileProg env (StructField <$> loc <*> pure "member10") m10
    compileProg env (StructField <$> loc <*> pure "member11") m11
    compileProg env (StructField <$> loc <*> pure "member12") m12
compileProg env loc (In (App Ut.Tup13 _ [m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13])) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
    compileProg env (StructField <$> loc <*> pure "member5") m5
    compileProg env (StructField <$> loc <*> pure "member6") m6
    compileProg env (StructField <$> loc <*> pure "member7") m7
    compileProg env (StructField <$> loc <*> pure "member8") m8
    compileProg env (StructField <$> loc <*> pure "member9") m9
    compileProg env (StructField <$> loc <*> pure "member10") m10
    compileProg env (StructField <$> loc <*> pure "member11") m11
    compileProg env (StructField <$> loc <*> pure "member12") m12
    compileProg env (StructField <$> loc <*> pure "member13") m13
compileProg env loc (In (App Ut.Tup14 _ [m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14])) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
    compileProg env (StructField <$> loc <*> pure "member5") m5
    compileProg env (StructField <$> loc <*> pure "member6") m6
    compileProg env (StructField <$> loc <*> pure "member7") m7
    compileProg env (StructField <$> loc <*> pure "member8") m8
    compileProg env (StructField <$> loc <*> pure "member9") m9
    compileProg env (StructField <$> loc <*> pure "member10") m10
    compileProg env (StructField <$> loc <*> pure "member11") m11
    compileProg env (StructField <$> loc <*> pure "member12") m12
    compileProg env (StructField <$> loc <*> pure "member13") m13
    compileProg env (StructField <$> loc <*> pure "member14") m14
compileProg env loc (In (App Ut.Tup15 _ [m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15])) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
    compileProg env (StructField <$> loc <*> pure "member5") m5
    compileProg env (StructField <$> loc <*> pure "member6") m6
    compileProg env (StructField <$> loc <*> pure "member7") m7
    compileProg env (StructField <$> loc <*> pure "member8") m8
    compileProg env (StructField <$> loc <*> pure "member9") m9
    compileProg env (StructField <$> loc <*> pure "member10") m10
    compileProg env (StructField <$> loc <*> pure "member11") m11
    compileProg env (StructField <$> loc <*> pure "member12") m12
    compileProg env (StructField <$> loc <*> pure "member13") m13
    compileProg env (StructField <$> loc <*> pure "member14") m14
    compileProg env (StructField <$> loc <*> pure "member15") m15
-- Special case foreign imports since they can be of void type and just have effects.
compileProg env loc (In (App p@Ut.ForeignImport{} t es)) = do
    es' <- mapM (compileExpr env) es
    tellProg [Assign loc $ fun' (compileTypeRep (opts env) t) (compileOp p) es']
-- Common nodes
compileProg env (Just loc) (In (App (Ut.Call f name) _ es)) = do
  es' <- mapM (compileExpr env) es
  let args = nub $ map exprToVar es' ++ fv loc
  tellProg [iVarInitCond f (AddrOf loc)]
  tellProg [spawn f name args]
compileProg env loc e = compileExprLoc env loc e


compileExpr :: CompileEnv -> Ut.UntypedFeld -> CodeWriter (Expression ())
-- Array
compileExpr env (In (App Ut.GetLength _ [a])) = do
   aExpr <- compileExpr env a
   return $ arrayLength aExpr
compileExpr env (In (App Ut.GetIx _ [arr, i])) = do
   a' <- compileExpr env arr
   i' <- compileExpr env i
   return $ ArrayElem a' i'
-- Bits
compileExpr env (In (App Ut.Bit t [arr])) = do
   a' <- compileExpr env arr
   let t' = compileTypeRep (opts env) t
   return $ binop t' "<<" (litI t' 1) a'
-- Binding
compileExpr env (In (Ut.Variable (Ut.Var v t))) = do
        env' <- ask
        case lookup v (alias env') of
          Nothing -> return $ mkVar (compileTypeRep (opts env) t) v
          Just e  -> return e
compileExpr env (In (Ut.App Ut.Let _ [a, In (Ut.Lambda (Ut.Var v ta) body)])) = do
    e <- compileLet env a ta v
    withAlias v e $ compileExpr env body
-- Bits
-- Condition
-- Conversion
compileExpr env (In (App Ut.F2I t es)) = do
    es' <- mapM (compileExpr env) es
    let f' = fun (Rep.MachineVector 1 Rep.FloatType) "truncf" es'
    return $ Cast (compileTypeRep (opts env) t) f'
compileExpr env (In (App Ut.I2N t1 [e]))
 | (Rep.MachineVector 1 (Rep.ComplexType t)) <- t'
 = do
    e' <- compileExpr env e
    let args = [Cast t e', litF 0]
    return $ fun t' (extend c99 "complex" t) args
 | otherwise = do
    e' <- compileExpr env e
    return $ Cast t' e'
  where t' = compileTypeRep (opts env) t1
compileExpr env (In (App Ut.B2I t [e])) = do
    e' <- compileExpr env e
    return $ Cast (compileTypeRep (opts env) t) e'
compileExpr env (In (App Ut.Round t es)) = do
    es' <- mapM (compileExpr env) es
    let f' = fun (Rep.MachineVector 1 Rep.FloatType) "roundf" es'
    return $ Cast (compileTypeRep (opts env) t) f'
compileExpr env (In (App Ut.Ceiling t es)) = do
    es' <- mapM (compileExpr env) es
    let f' = fun (Rep.MachineVector 1 Rep.FloatType) "ceilf" es'
    return $ Cast (compileTypeRep (opts env) t) f'
compileExpr env (In (App Ut.Floor t es)) = do
    es' <- mapM (compileExpr env) es
    let f' = fun (Rep.MachineVector 1 Rep.FloatType) "floorf" es'
    return $ Cast (compileTypeRep (opts env) t) f'
-- Error
compileExpr env (In (App (Ut.Assert msg) _ [cond, a])) = do
    compileAssert env cond msg
    compileExpr env a
-- Eq
-- FFI
-- Floating
compileExpr _ (In (App Ut.Pi t [])) = error "No pi ready"
-- Fractional
-- Future
-- Literal
compileExpr env (In (Ut.Literal l)) = literal env l
-- Loop
-- Logic
-- Mutable
compileExpr env (In (App Ut.Run _ [ma])) = compileExpr env ma
-- MutableArray
compileExpr env (In (App Ut.ArrLength _ [arr])) = do
    a' <- compileExpr env arr
    return $ arrayLength a'
-- MutableReference
compileExpr env (In (App Ut.GetRef _ [r])) = compileExpr env r
-- NoInline
-- Num
-- Ord
-- SizeProp
compileExpr env (In (App Ut.PropSize _ [e])) = compileExpr env e
-- SourceInfo
compileExpr env (In (App (Ut.SourceInfo info) _ [a])) = do
    tellProg [Comment True info]
    compileExpr env a
-- Tuple
compileExpr env (In (App p _ [tup]))
  | p `elem` [ Ut.Sel1, Ut.Sel2, Ut.Sel3, Ut.Sel4, Ut.Sel5, Ut.Sel6, Ut.Sel7
             , Ut.Sel8, Ut.Sel9, Ut.Sel10, Ut.Sel11, Ut.Sel12, Ut.Sel13
             , Ut.Sel14, Ut.Sel15] = do
    tupExpr <- compileExpr env tup
    return $ StructField tupExpr ("member" ++ drop 3 (show p))
compileExpr env e@(In (App p _ _))
 | p `elem` [ Ut.Parallel, Ut.Sequential, Ut.Condition, Ut.ConditionM
            , Ut.MkFuture, Ut.Await, Ut.Then, Ut.For, Ut.SetArr
            , Ut.WhileLoop, Ut.ForLoop, Ut.RunMutableArray, Ut.NoInline
            , Ut.Switch, Ut.WithArray, Ut.Tup2, Ut.Tup3, Ut.Tup4, Ut.Tup5
            , Ut.Tup6, Ut.Tup7, Ut.Tup8, Ut.Tup9, Ut.Tup10, Ut.Tup11, Ut.Tup11
            , Ut.Tup12, Ut.Tup13, Ut.Tup14, Ut.Tup15]
 = compileProgFresh env e
compileExpr env (In (App p t es)) = do
    es' <- mapM (compileExpr env) es
    return $ fun' (compileTypeRep (opts env) t) (compileOp p) es'
compileExpr env e = compileProgFresh env e

compileLet :: CompileEnv -> Ut.UntypedFeld -> Ut.Type -> Integer ->
              CodeWriter (Expression ())
compileLet env a ta v = do
   let var = mkVar (compileTypeRep (opts env) ta) v
   declare var
   compileProg env (Just var) a
   return var

compileAssert :: CompileEnv -> Ut.UntypedFeld -> String -> CodeWriter ()
compileAssert env cond msg = do
    condExpr <- compileExpr env cond
    tellProg [call "assert" [ValueParameter condExpr]]
    unless (null msg) $ tellProg [Comment False $ "{" ++ msg ++ "}"]

literal :: CompileEnv -> Ut.Lit -> CodeWriter (Expression ())
literal _   t@LUnit       = return (ConstExpr $ literalConst t)
literal _   t@LBool{}     = return (ConstExpr $ literalConst t)
literal _   t@LInt{}      = return (ConstExpr $ literalConst t)
literal _   t@LFloat{}    = return (ConstExpr $ literalConst t)
literal _   t@LDouble{}   = return (ConstExpr $ literalConst t)
literal _   t@LComplex{}  = return (ConstExpr $ literalConst t)
literal _   t@LArray{}    = return (ConstExpr $ literalConst t)
literal env t = do loc <- freshVar (opts env) "x" (typeof t)
                   literalLoc env loc t
                   return loc

-- | Returns true if we can represent the literal in Program.
representableType :: Ut.Lit -> Bool
representableType l
  | Ut.ArrayType{} <- t = True
  -- Simple types.
  | Ut.IntType{} <- t = True
  | Ut.ComplexType{} <- t = True
  | otherwise
  = t `elem` [Ut.UnitType, Ut.DoubleType, Ut.FloatType, Ut.BoolType]
      where t = typeof l

literalConst :: Ut.Lit -> Constant ()
literalConst LUnit          = IntConst 0 (Rep.NumType Ut.Unsigned Ut.S32)
literalConst (LBool a)      = BoolConst a
literalConst (LInt s sz a)  = IntConst (toInteger a) (Rep.NumType s sz)
literalConst (LFloat a)     = FloatConst a
literalConst (LDouble a)    = DoubleConst a
literalConst (LArray _ es)  = ArrayConst $ map literalConst es
literalConst (LComplex r i) = ComplexConst (literalConst r) (literalConst i)

literalLoc :: CompileEnv -> Expression () -> Ut.Lit -> CodeWriter ()
literalLoc _ loc arr@Ut.LArray{}
    = tellProg [copyProg (Just loc) [ConstExpr $ literalConst arr]]

literalLoc env loc (Ut.LTup2 ta tb) =
    do literalLoc env (StructField loc "member1") ta
       literalLoc env (StructField loc "member2") tb

literalLoc env loc (Ut.LTup3 ta tb tc) =
    do literalLoc env (StructField loc "member1") ta
       literalLoc env (StructField loc "member2") tb
       literalLoc env (StructField loc "member3") tc

literalLoc env loc (Ut.LTup4 ta tb tc td) =
    do literalLoc env (StructField loc "member1") ta
       literalLoc env (StructField loc "member2") tb
       literalLoc env (StructField loc "member3") tc
       literalLoc env (StructField loc "member4") td

literalLoc env loc (Ut.LTup5 ta tb tc td te) =
    do literalLoc env (StructField loc "member1") ta
       literalLoc env (StructField loc "member2") tb
       literalLoc env (StructField loc "member3") tc
       literalLoc env (StructField loc "member4") td
       literalLoc env (StructField loc "member5") te

literalLoc env loc (Ut.LTup6 ta tb tc td te tf) =
    do literalLoc env (StructField loc "member1") ta
       literalLoc env (StructField loc "member2") tb
       literalLoc env (StructField loc "member3") tc
       literalLoc env (StructField loc "member4") td
       literalLoc env (StructField loc "member5") te
       literalLoc env (StructField loc "member6") tf

literalLoc env loc (Ut.LTup7 ta tb tc td te tf tg) =
    do literalLoc env (StructField loc "member1") ta
       literalLoc env (StructField loc "member2") tb
       literalLoc env (StructField loc "member3") tc
       literalLoc env (StructField loc "member4") td
       literalLoc env (StructField loc "member5") te
       literalLoc env (StructField loc "member6") tf
       literalLoc env (StructField loc "member7") tg

literalLoc env loc (Ut.LTup8 ta tb tc td te tf tg th) =
    do literalLoc env (StructField loc "member1") ta
       literalLoc env (StructField loc "member2") tb
       literalLoc env (StructField loc "member3") tc
       literalLoc env (StructField loc "member4") td
       literalLoc env (StructField loc "member5") te
       literalLoc env (StructField loc "member6") tf
       literalLoc env (StructField loc "member7") tg
       literalLoc env (StructField loc "member8") th

literalLoc env loc (Ut.LTup9 ta tb tc td te tf tg th ti) =
    do literalLoc env (StructField loc "member1") ta
       literalLoc env (StructField loc "member2") tb
       literalLoc env (StructField loc "member3") tc
       literalLoc env (StructField loc "member4") td
       literalLoc env (StructField loc "member5") te
       literalLoc env (StructField loc "member6") tf
       literalLoc env (StructField loc "member7") tg
       literalLoc env (StructField loc "member8") th
       literalLoc env (StructField loc "member9") ti

literalLoc env loc (Ut.LTup10 ta tb tc td te tf tg th ti tj) =
    do literalLoc env (StructField loc "member1") ta
       literalLoc env (StructField loc "member2") tb
       literalLoc env (StructField loc "member3") tc
       literalLoc env (StructField loc "member4") td
       literalLoc env (StructField loc "member5") te
       literalLoc env (StructField loc "member6") tf
       literalLoc env (StructField loc "member7") tg
       literalLoc env (StructField loc "member8") th
       literalLoc env (StructField loc "member9") ti
       literalLoc env (StructField loc "member10") tj

literalLoc env loc (Ut.LTup11 ta tb tc td te tf tg th ti tj tk) =
    do literalLoc env (StructField loc "member1") ta
       literalLoc env (StructField loc "member2") tb
       literalLoc env (StructField loc "member3") tc
       literalLoc env (StructField loc "member4") td
       literalLoc env (StructField loc "member5") te
       literalLoc env (StructField loc "member6") tf
       literalLoc env (StructField loc "member7") tg
       literalLoc env (StructField loc "member8") th
       literalLoc env (StructField loc "member9") ti
       literalLoc env (StructField loc "member10") tj
       literalLoc env (StructField loc "member11") tk

literalLoc env loc (Ut.LTup12 ta tb tc td te tf tg th ti tj tk tl) =
    do literalLoc env (StructField loc "member1") ta
       literalLoc env (StructField loc "member2") tb
       literalLoc env (StructField loc "member3") tc
       literalLoc env (StructField loc "member4") td
       literalLoc env (StructField loc "member5") te
       literalLoc env (StructField loc "member6") tf
       literalLoc env (StructField loc "member7") tg
       literalLoc env (StructField loc "member8") th
       literalLoc env (StructField loc "member9") ti
       literalLoc env (StructField loc "member10") tj
       literalLoc env (StructField loc "member11") tk
       literalLoc env (StructField loc "member12") tl

literalLoc env loc (Ut.LTup13 ta tb tc td te tf tg th ti tj tk tl tm) =
    do literalLoc env (StructField loc "member1") ta
       literalLoc env (StructField loc "member2") tb
       literalLoc env (StructField loc "member3") tc
       literalLoc env (StructField loc "member4") td
       literalLoc env (StructField loc "member5") te
       literalLoc env (StructField loc "member6") tf
       literalLoc env (StructField loc "member7") tg
       literalLoc env (StructField loc "member8") th
       literalLoc env (StructField loc "member9") ti
       literalLoc env (StructField loc "member10") tj
       literalLoc env (StructField loc "member11") tk
       literalLoc env (StructField loc "member12") tl
       literalLoc env (StructField loc "member13") tm

literalLoc env loc (Ut.LTup14 ta tb tc td te tf tg th ti tj tk tl tm tn) =
    do literalLoc env (StructField loc "member1") ta
       literalLoc env (StructField loc "member2") tb
       literalLoc env (StructField loc "member3") tc
       literalLoc env (StructField loc "member4") td
       literalLoc env (StructField loc "member5") te
       literalLoc env (StructField loc "member6") tf
       literalLoc env (StructField loc "member7") tg
       literalLoc env (StructField loc "member8") th
       literalLoc env (StructField loc "member9") ti
       literalLoc env (StructField loc "member10") tj
       literalLoc env (StructField loc "member11") tk
       literalLoc env (StructField loc "member12") tl
       literalLoc env (StructField loc "member13") tm
       literalLoc env (StructField loc "member14") tn

literalLoc env loc (Ut.LTup15 ta tb tc td te tf tg th ti tj tk tl tm tn to) =
    do literalLoc env (StructField loc "member1") ta
       literalLoc env (StructField loc "member2") tb
       literalLoc env (StructField loc "member3") tc
       literalLoc env (StructField loc "member4") td
       literalLoc env (StructField loc "member5") te
       literalLoc env (StructField loc "member6") tf
       literalLoc env (StructField loc "member7") tg
       literalLoc env (StructField loc "member8") th
       literalLoc env (StructField loc "member9") ti
       literalLoc env (StructField loc "member10") tj
       literalLoc env (StructField loc "member11") tk
       literalLoc env (StructField loc "member12") tl
       literalLoc env (StructField loc "member13") tm
       literalLoc env (StructField loc "member14") tn
       literalLoc env (StructField loc "member15") to

literalLoc env loc t =
    do rhs <- literal env t
       assign (Just loc) rhs

chaseTree :: CompileEnv -> Location -> Ut.UntypedFeld -> Ut.UntypedFeld
            -> CodeWriter [(Pattern (), Block ())]
chaseTree env loc _s (In (App Ut.Condition _ [In (App Ut.Equal _ [c, a]), t, f]))
    -- , alphaEq s a -- TODO check that the scrutinees are equal
    = do
         e <- compileExpr env c
         (_,body) <- confiscateBlock $ compileProg env loc t
         cases <- chaseTree env loc _s f
         return $ (Pat e, body) : cases

chaseTree env loc _ a = do
    (_,body) <- confiscateBlock $ compileProg env loc a
    return [(PatDefault, body)]

-- | Chase down the right-spine of `Bind` and `Then` constructs and return
-- the last term
chaseBind :: Ut.UntypedFeld -> Ut.UntypedFeld
chaseBind (In (App Ut.Bind _ [_, In (Ut.Lambda _  body)])) = chaseBind body
chaseBind (In (App Ut.Then _ [_, body]))                   = chaseBind body
chaseBind a                                                = a

{- NOTES:

The trick of doing a copy at the end, i.e. `tellProg [copyProg loc
e]`, when compiling WithArray is important. It allows us to safely
return the pure array that is passed in as input. This is nice because
it allows us to implement `freezeArray` in terms of `withArray`.
In most cases I expect `withArray` to return a scalar as its final
result and then the copyProg is harmless.
-}

compileBind :: CompileEnv -> (Ut.Var, Ut.UntypedFeld) -> CodeWriter ()
compileBind env (Ut.Var v t, e) = do
   let var = mkVar (compileTypeRep (opts env) t) v
   declare var
   compileProg env (Just var) e

-- | Translates Op names to strings.
compileOp :: Ut.Op -> String
-- Bits
compileOp Ut.BAnd              = "&"
compileOp Ut.BOr               = "|"
compileOp Ut.BXor              = "^"
-- Complex
compileOp Ut.RealPart          = "creal"
compileOp Ut.ImagPart          = "cimag"
compileOp Ut.Sign              = "signum"
  -- Eq
compileOp Ut.Equal             = "=="
compileOp Ut.NotEqual          = "/="
  -- FFI
compileOp (Ut.ForeignImport s) = s
  -- Floating
compileOp Ut.Exp               = "exp"
  -- Fractional
compileOp Ut.DivFrac           = "/"
  -- Integral
compileOp Ut.IExp              = "pow"
  -- Logic
compileOp Ut.And               = "&&"
compileOp Ut.Or                = "||"
  -- Num
compileOp Ut.Add               = "+"
compileOp Ut.Sub               = "-"
compileOp Ut.Mul               = "*"
  -- Ord
compileOp Ut.LTH               = "<"
compileOp Ut.GTH               = ">"
compileOp Ut.LTE               = "<="
compileOp Ut.GTE               = ">="
compileOp p                    = toLower h:t
    where (h:t) = show p
