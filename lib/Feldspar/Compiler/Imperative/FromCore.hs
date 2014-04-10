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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore (
    fromCore
  , getCore'
  )
  where

import Data.Char (toLower)
import Data.List (nub, partition, last)
import Data.Maybe (isJust, fromJust)

import Control.Monad (unless)
import Control.Monad.RWS
import Control.Applicative

import Feldspar.Core.Types
import Feldspar.Core.UntypedRepresentation (Term(..), Lit(..), collectLetBinders)
import Feldspar.Core.UntypedRepresentation
         ( UntypedFeldF(PrimApp0), UntypedFeldF(PrimApp1)
         , UntypedFeldF(PrimApp2), UntypedFeldF(PrimApp3)
         )
import qualified Feldspar.Core.UntypedRepresentation as Ut
import Feldspar.Core.Middleend.FromTyped
import Feldspar.Core.Constructs (SyntacticFeld(..))
import Feldspar.Core.Frontend (reifyFeld)
import Feldspar.Range (upperBound)

import qualified Feldspar.Compiler.Imperative.Representation as Rep (Variable(..), Type(..), ScalarType(..))
import Feldspar.Compiler.Imperative.Representation
         ( ActualParameter(..), Block(..), Declaration(..), Entity(..)
         , Expression(..), Module(..), Program(..), Pattern(..)
         , FunctionMode(..), Constant(..), typeof, fv
         )
import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation
import Feldspar.Compiler.Backend.C.Options (Options(..))

-- Module that translates from the Syntactic program from the frontend to the
-- module format in the backend.

-- | Get the generated core for a program with a specified output name.
fromCore :: SyntacticFeld a => Options -> String -> a -> Module ()
fromCore opt funname prog = Module defs
  where
    (outParam,results) = evalRWS (compileProgTop opt funname ast) (initReader opt) initState
    ast        = untype $ reifyFeld (frontendOpts opt) N32 prog
    decls      = decl results
    ins        = params results
    post       = epilogue results
    Block ds p = block results
    paramTypes = getTypes $ Declaration outParam Nothing:map (`Declaration` Nothing) ins
    defs       =  nub (def results ++ paramTypes)
               ++ [Proc funname ins [outParam] $ Just (Block (ds ++ decls) (Sequence (p:post)))]

-- | Get the generated core for a program.
getCore' :: SyntacticFeld a => Options -> a -> Module ()
getCore' opts = fromCore opts "test"

compileProgTop :: Options -> String -> Ut.UntypedFeld ->
                  CodeWriter (Rep.Variable ())
compileProgTop opt funname (In (Ut.Lambda (Ut.Var v ta) body)) = do
  let typ = compileTypeRep ta
      (arg,arge) | Rep.StructType{} <- typ = (mkPointer typ v, Deref $ varToExpr arg)
                 | otherwise               = (mkVariable typ v, varToExpr arg)
  tell $ mempty {params=[arg]}
  withAlias v arge $
     compileProgTop opt funname body
compileProgTop opt funname (In (Ut.Let (In (Ut.Literal l)) (In (Ut.Lambda (Ut.Var v _) body))))
  = do tellDef [ValueDef var c]
       withAlias v (varToExpr var) $
         compileProgTop opt funname body
  where
    var = mkVariable (typeof c) v -- Note [Precise size information]
    c   = literalConst l
compileProgTop _ _ a = do
  let outType    = Rep.Pointer $ compileTypeRep (typeof a)
      outParam   = Rep.Variable outType "out"
      outLoc     = Deref $ varToExpr outParam
  compileProg cenv0 (Just outLoc) a
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
    { inTask :: Bool
    }

-- | Initial environment for compile.
cenv0 :: CompileEnv
cenv0 = CEnv False

-- | Compiles code and assigns the expression to the given location.
compileExprLoc :: CompileEnv -> Location  -> Ut.UntypedFeld  -> CodeWriter ()
compileExprLoc env loc e = do
    expr <- compileExpr env e
    assign loc expr

-- | Compiles code into a fresh variable.
compileProgFresh :: CompileEnv -> Ut.UntypedFeld -> CodeWriter (Expression ())
compileProgFresh env e = do
    loc <- freshVar "e" (typeof e)
    compileProg env (Just loc) e
    return loc

-- Compile an expression and make sure that the result is stored in a variable
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

mkLength :: CompileEnv -> Ut.UntypedFeld -> Ut.Type -> CodeWriter (Expression ())
mkLength env a t
  | isVariableOrLiteral a = compileExpr env a
  | otherwise             = do
      lenvar    <- freshVar "len" t
      compileProg env (Just lenvar) a
      return lenvar

mkBranch :: CompileEnv -> Location -> Ut.UntypedFeld -> Ut.UntypedFeld
         -> Maybe (Ut.UntypedFeld) -> CodeWriter ()
mkBranch env loc c th el = do
    ce <- compileExpr env c
    (_, tb) <- confiscateBlock $ compileProg env loc th
    (_, eb) <- if isJust el
                  then confiscateBlock $ compileProg env loc (fromJust el)
                  else return (undefined, toBlock Empty)
    tellProg [Switch ce [(Pat (litB True), tb), (Pat (litB False), eb)]]

compileProg :: CompileEnv -> Location -> Ut.UntypedFeld -> CodeWriter ()
-- Array
compileProg env loc (In (PrimApp2 Ut.Parallel _ len (In (Ut.Lambda (Ut.Var v ta) ixf)))) = do
   let ix = mkVar (compileTypeRep ta) v
   len' <- mkLength env len ta
   (_, b) <- confiscateBlock $ compileProg env (ArrayElem <$> loc <*> pure ix) ixf
   tellProg [initArray loc len']
   tellProg [for True (lName ix) len' (litI32 1) b]
compileProg env loc (In (PrimApp3 Ut.Sequential _ len init' (In (Ut.Lambda (Ut.Var v tix) ixf1))))
   | In (Ut.Lambda (Ut.Var s tst) l) <- ixf1
   , (bs, In (Ut.Tup2 (In (Ut.Variable t1)) (In (Ut.Variable t2)))) <- collectLetBinders l
   , not $ null bs
   , (e, step) <- last bs
   , t1 == e
   , t2 == e
   = do
        blocks <- mapM (confiscateBlock . (compileBind env)) (init bs)
        let (dss, lets) = unzip $ map (\(_, Block ds (Sequence body)) -> (ds, body)) blocks
        let ix = mkVar (compileTypeRep tix) v
        len' <- mkLength env len tix
        st1 <- freshVar "st" tst
        let st = mkRef (compileTypeRep tst) s
            st_val = Deref st
        declareAlias st
        (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s st_val $ compileProg env (ArrayElem <$> loc <*> pure ix) step
        withAlias s st_val $ compileProg env (Just st1) init'
        tellProg [ Assign st (AddrOf st1)
                 , initArray loc len']
        tellProg [toProg $ Block (concat dss ++ ds) $
                  for False (lName ix) len' (litI32 1) $
                               toBlock $ Sequence (concat lets ++ body ++ maybe [] (\arr -> [Assign st $ AddrOf (ArrayElem arr ix)]) loc)]
compileProg env loc (In (PrimApp3 Ut.Sequential _ len st (In (Ut.Lambda (Ut.Var v t) (In (Ut.Lambda (Ut.Var s _) step))))))
  = do
       let tr' = typeof step
       let ix = mkVar (compileTypeRep t) v
       len' <- mkLength env len t
       tmp  <- freshVar "seq" tr'
       (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s (StructField tmp "member2") $ compileProg env (Just tmp) step
       tellProg [initArray loc len']
       compileProg env (Just $ StructField tmp "member2") st
       tellProg [toProg $ Block ds $
                 for False (lName ix) len' (litI32 1) $ toBlock $
                   Sequence $ body ++
                     [copyProg (ArrayElem <$> loc <*> pure ix) [StructField tmp "member1"]
                     ]]
compileProg env loc (In (PrimApp2 Ut.Append _ a b)) = do
   a' <- compileExpr env a
   b' <- compileExpr env b
   tellProg [copyProg loc [a', b']]
compileProg env loc (In (PrimApp3 Ut.SetIx _ arr i a)) = do
   compileProg env loc arr
   i' <- compileExpr env i
   compileProg env (ArrayElem <$> loc <*> pure i') a
compileProg env (Just loc) (In (PrimApp2 Ut.GetIx _ arr i)) = do
   a' <- compileExpr env arr
   i' <- compileExpr env i
   let el = ArrayElem a' i'
   tellProg $ if isArray $ typeof el
                then [Assign loc el]
                else [copyProg (Just loc) [el]]
compileProg env loc (In (PrimApp2 Ut.SetLength _ len arr)) = do
   len' <- compileExpr env len
   compileProg env loc arr
   tellProg [setLength loc len']
-- Binding
compileProg _   _   (In Ut.Lambda{}) = error "Can only compile top-level lambda"
compileProg env loc (In (Ut.Let a (In (Ut.Lambda (Ut.Var v ta) body)))) = do
   e <- compileLet env a ta v
   withAlias v e $ compileProg env loc body
-- Bits
-- Complex
-- Condition
compileProg env loc (In (PrimApp3 Ut.Condition _ cond tHEN eLSE)) =
   mkBranch env loc cond tHEN $ Just eLSE
compileProg env loc (In (PrimApp3 Ut.ConditionM _ cond tHEN eLSE)) =
   mkBranch env loc cond tHEN $ Just eLSE
-- Conversion
-- Elements
compileProg env loc (In (PrimApp2 Ut.EMaterialize t len arr)) = do
   len' <- mkLength env len t
   tellProg [initArray loc len']
   compileProg env loc arr
compileProg env (Just loc) (In (PrimApp2 Ut.EWrite _ ix e)) = do
   dst <- compileExpr env ix
   compileProg env (Just $ ArrayElem loc dst) e
compileProg _   _   (In (PrimApp0 Ut.ESkip _)) = return ()
compileProg env loc (In (PrimApp2 Ut.EPar _ p1 p2)) = do
   (_, Block ds1 b1) <- confiscateBlock $ compileProg env loc p1
   (_, Block ds2 b2) <- confiscateBlock $ compileProg env loc p2
   tellProg [toProg $ Block (ds1 ++ ds2) (Sequence [b1,b2])]
compileProg env loc (In (PrimApp2 Ut.EparFor _ len (In (Ut.Lambda (Ut.Var v ta) ixf)))) = do
   let ix = mkVar (compileTypeRep ta) v
   len' <- mkLength env len ta
   (_, ixf') <- confiscateBlock $ compileProg env loc ixf
   tellProg [for True (lName ix) len' (litI32 1) ixf']
-- Error
compileProg _   _   (In (PrimApp0 Ut.Undefined _)) = return ()
compileProg env loc (In (PrimApp2 (Ut.Assert msg) _ cond a)) = do
   compileAssert env cond msg
   compileProg env loc a
-- Future
compileProg env (Just loc) (In (PrimApp1 Ut.MkFuture _ p)) = do
   env' <- ask
   let args = nub $ [case lookup v (alias env') of
                    Nothing -> mkVariable (compileTypeRep t) v
                    Just (VarExpr e) -> e
                    -- Variables that got a Pointer wrapped around
                    -- their type in FromCore.
                    Just (Deref (VarExpr e)) -> e
              | (Ut.Var v t) <- Ut.fv p
              ] ++ fv loc
   -- Task core:
   ((_, ws), Block ds bl)  <- confiscateBigBlock $ do
       p' <- compileExprVar env {inTask = True} p
       tellProg [iVarPut loc p']
   funId  <- freshId
   let coreName = "task_core" ++ show funId
   tellDef [Proc coreName args [] $ Just (Block (decl ws ++ ds) bl)]
   -- Task:
   let taskName = "task" ++ show funId
       runTask = Just $ toBlock $ run coreName args
   tellDef [Proc taskName [] [mkNamedRef "params" Rep.VoidType (-1)] runTask]
   -- Spawn:
   tellProg [iVarInit (AddrOf loc)]
   tellProg [spawn taskName args]
compileProg env loc (In (PrimApp1 Ut.Await _ a)) = do
   fut <- compileExprVar env a
   tellProg [iVarGet (inTask env) l fut | Just l <- [loc]]
-- Literal
compileProg _ loc (In (Ut.Literal a)) = case loc of
     Just l -> literalLoc l a
     Nothing -> return ()
-- Logic
-- Loop
compileProg env (Just loc) (In (PrimApp3 Ut.ForLoop _ len init (In (Ut.Lambda (Ut.Var ix ta) (In (Ut.Lambda (Ut.Var st stt) ixf))))))
  = do
      let ix' = mkVar (compileTypeRep ta) ix
          stvar = mkVar (compileTypeRep (typeof ixf)) st
      len' <- mkLength env len ta
      (lstate, stvar) <- mkDoubleBufferState loc st
      compileProg env (Just lstate) init
      (_, Block ds body) <- withAlias st lstate $ confiscateBlock
                          $ compileProg env (Just stvar) ixf >> (shallowCopyWithRefSwap lstate stvar)
      tellProg [toProg $ Block ds (for False (lName ix') len' (litI32 1) (toBlock body))]
      shallowAssign (Just loc) lstate
compileProg env (Just loc) (In (PrimApp3 Ut.WhileLoop t init (In (Ut.Lambda (Ut.Var cv ct) cond)) e@(In (Ut.Lambda (Ut.Var bv bt) body)))) = do
    let stvar = mkVar (compileTypeRep bt) bv
        condv = mkVar (compileTypeRep (typeof cond)) cv
    (lstate,stvar) <- mkDoubleBufferState loc bv
    compileProg env (Just lstate) init
    (_, cond') <- confiscateBlock $ withAlias cv lstate $ compileProg env (Just condv) cond
    (_, body') <- withAlias bv lstate $ confiscateBlock $ compileProg env (Just stvar) body >> shallowCopyWithRefSwap lstate stvar
    declare condv
    tellProg [while cond' condv body']
    shallowAssign (Just loc) lstate
-- LoopM
compileProg env loc (In (PrimApp2 Ut.While _ (In (Ut.Lambda _ cond)) step)) = do
   condv <- freshVar "cond" (typeof cond)
   (_, cond') <- confiscateBlock $ compileProg env (Just condv) cond
   (_, step') <- confiscateBlock $ compileProg env loc step
   tellProg [while cond' condv step']
compileProg env loc (In (PrimApp2 Ut.For _ len (In (Ut.Lambda (Ut.Var v ta) ixf)))) = do
   let ix = mkVar (compileTypeRep ta) v
   len' <- mkLength env len ta
   (_, Block ds body) <- confiscateBlock $ compileProg env loc ixf
   tellProg [toProg $ Block ds (for False (lName ix) len' (litI32 1) (toBlock body))]
-- Mutable
compileProg env loc (In (PrimApp1 Ut.Run _ ma)) = compileProg env loc ma
compileProg env loc (In (PrimApp1 Ut.Return t a))
  | Ut.MutType Ut.UnitType <- t = return ()
  | Ut.ParType Ut.UnitType <- t = return ()
  | otherwise = compileProg env loc a
compileProg env loc (In (PrimApp2 Ut.Bind _ ma (In (Ut.Lambda (Ut.Var v ta) body))))
  | (In (PrimApp0 Ut.ParNew _)) <- ma = do
   let var = mkVar (compileTypeRep ta) v
   declare var
   tellProg [iVarInit (AddrOf var)]
   compileProg env loc body
  | otherwise = do
   let var  = mkVar (compileTypeRep ta) v
   declare var
   compileProg env (Just var) ma
   compileProg env loc body
compileProg env loc (In (PrimApp2 Ut.Then _ ma mb)) = do
   compileProg env Nothing ma
   compileProg env loc mb
compileProg env loc (In (PrimApp2 Ut.When _ c action)) =
   mkBranch env loc c action Nothing
-- MutableArray
compileProg env loc (In (PrimApp2 Ut.NewArr _ len a)) = do
   nId <- freshId
   let ix = varToExpr $ mkNamedVar "i" (Rep.MachineVector 1 (Rep.NumType Ut.Unsigned Ut.S32)) nId
   a' <- compileExpr env a
   l  <- compileExpr env len
   tellProg [initArray loc l]
   tellProg [for False "i" l (litI32 1) $ toBlock (Sequence [copyProg (ArrayElem <$> loc <*> pure ix) [a']])]
compileProg env loc (In (PrimApp1 Ut.NewArr_ _ len)) = do
   l <- compileExpr env len
   tellProg [initArray loc l]
compileProg env loc (In (PrimApp2 Ut.GetArr _ arr i)) = do
   arr' <- compileExpr env arr
   i'   <- compileExpr env i
   assign loc (ArrayElem arr' i')
compileProg env loc (In (PrimApp3 Ut.SetArr _ arr i a)) = do
   arr' <- compileExpr env arr
   i'   <- compileExpr env i
   a'   <- compileExpr env a
   assign (Just $ ArrayElem arr' i') a'
-- MutableReference
compileProg env loc (In (PrimApp1 Ut.NewRef _ a)) = compileProg env loc a
compileProg env loc (In (PrimApp1 Ut.GetRef _ r)) = compileProg env loc r
compileProg env loc (In (PrimApp2 Ut.SetRef _ r a)) = do
   var  <- compileExpr env r
   compileProg env (Just var) a
compileProg env loc (In (PrimApp2 Ut.ModRef _ r (In (Ut.Lambda (Ut.Var v _) body)))) = do
   var <- compileExpr env r
   withAlias v var $ compileProg env (Just var) body
       -- Since the modifier function is pure it is safe to alias
       -- v with var here
-- MutableToPure
compileProg env (Just loc) (In (PrimApp1 Ut.RunMutableArray _ marr))
 | (In (PrimApp2 Ut.Bind _ (In (PrimApp1 Ut.NewArr_ _ l)) (In (Ut.Lambda (Ut.Var v _) body)))) <- marr
 , (In (PrimApp1 Ut.Return _ (In (Ut.Variable (Ut.Var r _))))) <- chaseBind body
 , v == r
 = do
     len <- compileExpr env l
     tellProg [setLength (Just loc) len]
     withAlias v loc $ compileProg env (Just loc) body
compileProg env loc (In (PrimApp1 Ut.RunMutableArray _ marr)) = compileProg env loc marr
compileProg env loc (In (PrimApp2 Ut.WithArray _ marr@(In Ut.Variable{}) (In (Ut.Lambda (Ut.Var v ta) body)))) = do
    e <- compileExpr env marr
    withAlias v e $ do
      b <- compileExpr env body
      tellProg [copyProg loc [b]]
compileProg env loc (In (PrimApp2 Ut.WithArray _ marr (In (Ut.Lambda (Ut.Var v ta) body)))) = do
    let var = mkVar (compileTypeRep ta) v
    declare var
    compileProg env (Just var) marr
    e <- compileExpr env body
    tellProg [copyProg loc [e]]
-- Noinline
compileProg env (Just loc) (In (PrimApp1 Ut.NoInline _ p)) = do
    let args = nub $ [mkVariable (compileTypeRep t) v
               | (Ut.Var v t) <- Ut.fv p
               ] ++ fv loc
    (_, b)  <- confiscateBlock $ compileProg env (Just loc) p
    let isInParam v = vName v /= lName loc
    let (ins,outs) = partition isInParam args
    funId  <- freshId
    let funname = "noinline" ++ show funId
    tellDef [Proc funname ins outs $ Just b]
    let ins' = map (\v -> ValueParameter $ varToExpr $ Rep.Variable (typeof v) (vName v)) ins
    tellProg [call funname $ ins' ++ [ValueParameter loc]]
-- Par
compileProg env loc (In (PrimApp1 Ut.ParRun _ p)) = compileProg env loc p
compileProg _   _   (In (PrimApp0 Ut.ParNew _)) = return ()
compileProg env loc (In (PrimApp1 Ut.ParGet _ r)) = do
    iv <- compileExpr env r
    tellProg [iVarGet (inTask env) l iv | Just l <- [loc]]
compileProg env loc (In (PrimApp2 Ut.ParPut _ r a)) = do
    iv  <- compileExpr env r
    val <- compileExpr env a
    i   <- freshId
    let var = varToExpr $ mkNamedVar "msg" (typeof val) i
    declare var
    assign (Just var) val
    tellProg [iVarPut iv var]
compileProg env loc (In (PrimApp1 Ut.ParFork _ p)) = do
   env' <- ask
   let args = nub $ [case lookup v (alias env') of
                     Nothing -> mkVariable (compileTypeRep t) v
                     Just (VarExpr e) -> e
                     Just (Deref (VarExpr e)) -> e
               | (Ut.Var v t) <- Ut.fv p
               ] ++ maybe [] fv loc
   -- Task core:
   ((_, ws), Block ds b) <- confiscateBigBlock $ compileProg env {inTask = True} loc p
   funId  <- freshId
   let coreName = "task_core" ++ show funId
   tellDef [Proc coreName args [] $ Just (Block (decl ws ++ ds) b)]
   -- Task:
   let taskName = "task" ++ show funId
       runTask = Just $ toBlock $ run coreName args
   tellDef [Proc taskName [] [mkNamedRef "params" Rep.VoidType (-1)] runTask]
   -- Spawn:
   tellProg [spawn taskName args]
compileProg env loc (In (PrimApp0 Ut.ParYield _)) = return ()
-- Save
compileProg env loc (In (PrimApp1 Ut.Save _ e)) = compileProg env loc e
-- SizeProp
compileProg env loc (In (PrimApp1 Ut.PropSize _ e)) = compileProg env loc e
-- SourceInfo
compileProg env loc (In (PrimApp1 (Ut.SourceInfo info) _ a)) = do
    tellProg [Comment True info]
    compileProg env loc a
-- Switch
compileProg env loc (In (PrimApp1 Ut.Switch _ tree@(In (PrimApp3 Ut.Condition _ (In (PrimApp2 Ut.Equal _ _ s)) _ _)))) = do
    scrutinee <- compileExpr env s
    alts      <- chaseTree env loc s tree
    tellProg [Switch{..}]
compileProg env loc (In (PrimApp1 Ut.Switch _ tree)) = compileProg env loc tree
-- Tuple
compileProg env loc (In (Ut.Tup2 m1 m2)) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
compileProg env loc (In (Ut.Tup3 m1 m2 m3)) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
compileProg env loc (In (Ut.Tup4 m1 m2 m3 m4)) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
compileProg env loc (In (Ut.Tup5 m1 m2 m3 m4 m5)) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
    compileProg env (StructField <$> loc <*> pure "member5") m5
compileProg env loc (In (Ut.Tup6 m1 m2 m3 m4 m5 m6)) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
    compileProg env (StructField <$> loc <*> pure "member5") m5
    compileProg env (StructField <$> loc <*> pure "member6") m6
compileProg env loc (In (Ut.Tup7 m1 m2 m3 m4 m5 m6 m7)) = do
    compileProg env (StructField <$> loc <*> pure "member1") m1
    compileProg env (StructField <$> loc <*> pure "member2") m2
    compileProg env (StructField <$> loc <*> pure "member3") m3
    compileProg env (StructField <$> loc <*> pure "member4") m4
    compileProg env (StructField <$> loc <*> pure "member5") m5
    compileProg env (StructField <$> loc <*> pure "member6") m6
    compileProg env (StructField <$> loc <*> pure "member7") m7
compileProg env loc e = compileExprLoc env loc e


compileExpr :: CompileEnv -> Ut.UntypedFeld -> CodeWriter (Expression ())
-- Array
compileExpr env (In (PrimApp1 Ut.GetLength _ a)) = do
   aExpr <- compileExpr env a
   return $ arrayLength aExpr
compileExpr env (In (PrimApp2 Ut.GetIx _ arr i)) = do
   a' <- compileExpr env arr
   i' <- compileExpr env i
   return $ ArrayElem a' i'
-- Binding
compileExpr env (In (Ut.Variable (Ut.Var v t))) = do
        env' <- ask
        case lookup v (alias env') of
          Nothing -> return $ mkVar (compileTypeRep t) v
          Just e  -> return e
compileExpr env (In (Ut.Let a (In (Ut.Lambda (Ut.Var v ta) body)))) = do
    e <- compileLet env a ta v
    withAlias v e $ compileExpr env body
-- Bits
compileExpr env (In (PrimApp2 o@Ut.BAnd t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
compileExpr env (In (PrimApp2 o@Ut.BOr t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
compileExpr env (In (PrimApp2 o@Ut.BXor t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
-- Error
compileExpr env (In (PrimApp2 (Ut.Assert msg) _ cond a)) = do
    compileAssert env cond msg
    compileExpr env a
-- Eq
compileExpr env (In (PrimApp2 o@Ut.Equal t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
compileExpr env (In (PrimApp2 o@Ut.NotEqual t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
-- FFI
compileExpr env (In (Ut.ForeignImport name t es)) = do
    es' <- mapM (compileExpr env) es
    return $ fun' Prefix (compileTypeRep t) True name es'
-- Floating
compileExpr env (In (PrimApp0 Ut.Pi t)) = error "No pi ready"
-- Fractional
compileExpr env (In (PrimApp2 o@Ut.DivFrac t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
-- Future
compileExpr env e@(In (PrimApp1 Ut.MkFuture _ _)) = compileProgFresh env e
compileExpr env e@(In (PrimApp1 Ut.Await _ _)) = compileProgFresh env e
-- Literal
compileExpr env (In (Ut.Literal l)) = literal l
-- Logic
compileExpr env (In (PrimApp2 o@Ut.And t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
compileExpr env (In (PrimApp2 o@Ut.Or t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
-- Mutable
compileExpr env (In (PrimApp1 Ut.Run _ ma)) = compileExpr env ma
-- MutableArray
compileExpr env (In (PrimApp1 Ut.ArrLength _ arr)) = do
    a' <- compileExpr env arr
    return $ arrayLength a'
compileExpr env e@(In (PrimApp2 Ut.WithArray _ _ _)) = compileProgFresh env e
-- MutableReference
compileExpr env (In (PrimApp1 Ut.GetRef _ r)) = compileExpr env r
-- MutableToPure
compileExpr env e@(In (PrimApp1 Ut.RunMutableArray _ _)) = compileProgFresh env e
-- NoInline
compileExpr env e@(In (PrimApp1 Ut.NoInline _ _)) = compileProgFresh env e
-- Num
compileExpr env (In (PrimApp2 o@Ut.Add t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
compileExpr env (In (PrimApp2 o@Ut.Sub t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
compileExpr env (In (PrimApp2 o@Ut.Mul t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
-- Ord
compileExpr env (In (PrimApp2 o@Ut.LTH t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
compileExpr env (In (PrimApp2 o@Ut.GTH t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
compileExpr env (In (PrimApp2 o@Ut.LTE t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
compileExpr env (In (PrimApp2 o@Ut.GTE t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Infix (compileTypeRep t) True (compileOp o) [e1', e2']
-- Save
compileExpr env (In (PrimApp1 Ut.Save _ e)) = compileExpr env e
-- SizeProp
compileExpr env (In (PrimApp1 Ut.PropSize _ e)) = compileExpr env e
-- SourceInfo
compileExpr env (In (PrimApp1 (Ut.SourceInfo info) _ a)) = do
    tellProg [Comment True info]
    compileExpr env a
-- Tuple
compileExpr env (In (PrimApp1 Ut.Sel1 _ tup)) = do
    tupExpr <- compileExpr env tup
    return $ StructField tupExpr "member1"
compileExpr env (In (PrimApp1 Ut.Sel2 _ tup)) = do
    tupExpr <- compileExpr env tup
    return $ StructField tupExpr "member2"
compileExpr env (In (PrimApp1 Ut.Sel3 _ tup)) = do
    tupExpr <- compileExpr env tup
    return $ StructField tupExpr "member3"
compileExpr env (In (PrimApp1 Ut.Sel4 _ tup)) = do
    tupExpr <- compileExpr env tup
    return $ StructField tupExpr "member4"
compileExpr env (In (PrimApp1 Ut.Sel5 _ tup)) = do
    tupExpr <- compileExpr env tup
    return $ StructField tupExpr "member5"
compileExpr env (In (PrimApp1 Ut.Sel6 _ tup)) = do
    tupExpr <- compileExpr env tup
    return $ StructField tupExpr "member6"
compileExpr env (In (PrimApp1 Ut.Sel7 _ tup)) = do
    tupExpr <- compileExpr env tup
    return $ StructField tupExpr "member7"
compileExpr env (In (PrimApp0 p t)) = do
    return $ fun' Prefix (compileTypeRep t) True (compileOp p) []
compileExpr env (In (PrimApp1 p t e)) = do
    e' <- compileExpr env e
    return $ fun' Prefix (compileTypeRep t) True (compileOp p) [e']
compileExpr env (In (PrimApp2 p t e1 e2)) = do
    e1' <- compileExpr env e1
    e2' <- compileExpr env e2
    return $ fun' Prefix (compileTypeRep t) True (compileOp p) [e1', e2']
compileExpr env e = compileProgFresh env e

compileLet :: CompileEnv -> Ut.UntypedFeld -> Ut.Type -> Integer ->
              CodeWriter (Expression ())
compileLet env a ta v = do
   let var = mkVar (compileTypeRep ta) v
   declare var
   compileProg env (Just var) a
   return var

compileAssert :: CompileEnv -> Ut.UntypedFeld -> String -> CodeWriter ()
compileAssert env cond msg = do
    condExpr <- compileExpr env cond
    tellProg [call "assert" [ValueParameter condExpr]]
    unless (null msg) $ tellProg [Comment False $ "{" ++ msg ++ "}"]

literal :: Ut.Lit -> CodeWriter (Expression ())
literal t@LUnit       = return (ConstExpr $ literalConst t)
literal t@LBool{}     = return (ConstExpr $ literalConst t)
literal t@LInt{}      = return (ConstExpr $ literalConst t)
literal t@LFloat{}    = return (ConstExpr $ literalConst t)
literal t@LDouble{}   = return (ConstExpr $ literalConst t)
literal t@LComplex{}  = return (ConstExpr $ literalConst t)
literal t@LArray{}    = return (ConstExpr $ literalConst t)
literal t = do loc <- freshVar "x" (typeof t)
               literalLoc loc t
               return loc

literalConst :: Ut.Lit -> Constant ()
literalConst LUnit          = IntConst 0 (Rep.NumType Ut.Unsigned Ut.S32)
literalConst (LBool a)      = BoolConst a
literalConst (LInt s sz a)  = IntConst (toInteger a) (Rep.NumType s sz)
literalConst (LFloat a)     = FloatConst a
literalConst (LDouble a)    = DoubleConst a
literalConst (LArray t es)  = ArrayConst $ map literalConst es
literalConst (LComplex r i) = ComplexConst (literalConst r) (literalConst i)

literalLoc :: Expression () -> Ut.Lit -> CodeWriter ()
literalLoc loc arr@Ut.LArray{}
    = tellProg [copyProg (Just loc) [ConstExpr $ literalConst arr]]

literalLoc loc (Ut.LTup2 ta tb) =
    do literalLoc (StructField loc "member1") ta
       literalLoc (StructField loc "member2") tb

literalLoc loc (Ut.LTup3 ta tb tc) =
    do literalLoc (StructField loc "member1") ta
       literalLoc (StructField loc "member2") tb
       literalLoc (StructField loc "member3") tc

literalLoc loc (Ut.LTup4 ta tb tc td) =
    do literalLoc (StructField loc "member1") ta
       literalLoc (StructField loc "member2") tb
       literalLoc (StructField loc "member3") tc
       literalLoc (StructField loc "member4") td

literalLoc loc (Ut.LTup5 ta tb tc td te) =
    do literalLoc (StructField loc "member1") ta
       literalLoc (StructField loc "member2") tb
       literalLoc (StructField loc "member3") tc
       literalLoc (StructField loc "member4") td
       literalLoc (StructField loc "member5") te

literalLoc loc (Ut.LTup6 ta tb tc td te tf) =
    do literalLoc (StructField loc "member1") ta
       literalLoc (StructField loc "member2") tb
       literalLoc (StructField loc "member3") tc
       literalLoc (StructField loc "member4") td
       literalLoc (StructField loc "member5") te
       literalLoc (StructField loc "member6") tf

literalLoc loc (Ut.LTup7 ta tb tc td te tf tg) =
    do literalLoc (StructField loc "member1") ta
       literalLoc (StructField loc "member2") tb
       literalLoc (StructField loc "member3") tc
       literalLoc (StructField loc "member4") td
       literalLoc (StructField loc "member5") te
       literalLoc (StructField loc "member6") tf
       literalLoc (StructField loc "member7") tg

literalLoc loc t =
    do rhs <- literal t
       assign (Just loc) rhs

chaseTree :: CompileEnv -> Location -> Ut.UntypedFeld -> Ut.UntypedFeld
            -> CodeWriter [(Pattern (), Block ())]
chaseTree env loc _s (In (PrimApp3 Ut.Condition _ (In (PrimApp2 Ut.Equal _ c  a)) t f))
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
chaseBind (In (PrimApp2 Ut.Bind _ _ (In (Ut.Lambda _  body)))) = chaseBind body
chaseBind (In (PrimApp2 Ut.Then _ _ body))                     = chaseBind body
chaseBind a                                         = a

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
   let var = mkVar (compileTypeRep t) v
   declare var
   compileProg env (Just var) e

-- Class for translating PrimOp names to strings.
class CompileOp a where
  compileOp :: Show a => a -> String

instance CompileOp Ut.PrimOp0 where
  compileOp p            = toLower h:t
    where (h:t) = show p

instance CompileOp Ut.PrimOp1 where
  compileOp Ut.RealPart  = "creal"
  compileOp Ut.ImagPart  = "cimag"
  compileOp Ut.F2I       = "f2i"
  compileOp Ut.I2N       = "i2n"
  compileOp Ut.B2I       = "b2i"
  compileOp Ut.Sign      = "signum"
  -- Floating
  compileOp Ut.Exp       = "exp"
  compileOp p            = toLower h:t
    where (h:t) = show p

instance CompileOp Ut.PrimOp2 where
  -- Bits
  compileOp Ut.BAnd      = "&"
  compileOp Ut.BOr       = "|"
  compileOp Ut.BXor      = "^"
  -- Eq
  compileOp Ut.Equal     = "=="
  compileOp Ut.NotEqual  = "/="
  -- Fractional
  compileOp Ut.DivFrac   = "/"
  -- Integral
  compileOp Ut.IExp      = "pow"
  -- Logic
  compileOp Ut.And       = "&&"
  compileOp Ut.Or        = "||"
  -- Num
  compileOp Ut.Add       = "+"
  compileOp Ut.Sub       = "-"
  compileOp Ut.Mul       = "*"
  -- Ord
  compileOp Ut.LTH       = "<"
  compileOp Ut.GTH       = ">"
  compileOp Ut.LTE       = "<="
  compileOp Ut.GTE       = "<="
  compileOp p            = toLower h:t
    where (h:t) = show p
