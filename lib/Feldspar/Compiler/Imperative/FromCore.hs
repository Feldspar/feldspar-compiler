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

module Feldspar.Compiler.Imperative.FromCore where

import Data.List (nub, partition, last)
import Data.Maybe (isJust, fromJust)

import Control.Monad (unless)
import Control.Monad.RWS
import Control.Applicative

import Feldspar.Core.Types
import Feldspar.Core.UntypedRepresentation (Term(..), Lit(..))
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

fromCore :: SyntacticFeld a => Options -> String -> a -> Module ()
fromCore opt funname prog = Module defs
  where
    (outParam,results) = evalRWS (compileProgTop opt funname [] ast) (initReader opt) initState
    ast        = untypeProg $ reifyFeld (frontendOpts opt) N32 prog
    decls      = decl results
    ins        = params results
    post       = epilogue results
    Block ds p = block results
    paramTypes = getTypes opt $ Declaration outParam Nothing:map (`Declaration` Nothing) ins
    defs       =  nub (def results ++ paramTypes)
               ++ [Proc funname ins [outParam] $ Just (Block (ds ++ decls) (Sequence (p:post)))]

-- | Get the generated core for a program.
getCore' :: SyntacticFeld a => Options -> a -> Module ()
getCore' opts = fromCore opts "test"

compileProgTop :: Options -> String -> [(Ut.Var, Ut.UntypedFeld)]
               -> Ut.UntypedFeld -> CodeWriter (Rep.Variable ())
compileProgTop opt funname bs (In (Ut.Lambda (Ut.Var v ta) body)) = do
  let typ = compileTypeRep ta
      (arg,arge) | Rep.StructType{} <- typ = (mkPointer typ v, Deref $ varToExpr arg)
                 | otherwise               = (mkVariable typ v, varToExpr arg)
  tell $ mempty {params=[arg]}
  withAlias v arge $
     compileProgTop opt funname bs body
-- Input on form let x = n in e
compileProgTop opt funname bs (In (Ut.Let e@(In Ut.Literal{}) (In (Ut.Lambda (Ut.Var v vt) body))))
  | [ProcedureCall "copy" [ValueParameter (VarExpr vr), ValueParameter (ConstExpr c)]] <- bd
  , freshName Prelude.== vName vr -- Ensure that compiled result is on form x = n
  = do tellDef [ValueDef var c]
       withAlias v (varToExpr var) $
         compileProgTop opt funname bs body
  where
    outType  = case compileTypeRep vt of
                 Rep.ArrayType rs t -> Rep.NativeArray (Just $ upperBound rs) t
                 t -> t
    var@(Rep.Variable _ freshName) = mkVariable outType v
    bd = sequenceProgs $ blockBody $ block $ snd $
          evalRWS (compileProg (Just $ varToExpr var) e) (initReader opt) initState
compileProgTop opt funname bs (In (Ut.Let e (In (Ut.Lambda v body))))
  = compileProgTop opt funname ((v, e):bs) body
compileProgTop _ _ bs a = do
  let outType    = Rep.Pointer $ compileTypeRep (typeof a)
      outParam   = Rep.Variable outType "out"
      outLoc     = Deref $ varToExpr outParam
  mapM_ compileBind (reverse bs)
  compileProg (Just outLoc) a
  return outParam

-- | Compiles code and assigns the expression to the given location.
compileExprLoc :: Location  -> Ut.UntypedFeld  -> CodeWriter ()
compileExprLoc loc e = do
    expr <- compileExpr e
    assign loc expr

-- | Compiles code into a fresh variable.
compileProgFresh :: Ut.UntypedFeld -> CodeWriter (Expression ())
compileProgFresh e = do
    loc <- freshVar "e" (typeof e)
    compileProg (Just loc) e
    return loc

-- Compile an expression and make sure that the result is stored in a variable
compileExprVar :: Ut.UntypedFeld -> CodeWriter (Expression ())
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

mkLength :: Ut.UntypedFeld -> Ut.Type -> CodeWriter (Expression ())
mkLength a t
  | isVariableOrLiteral a = compileExpr a
  | otherwise             = do
      lenvar    <- freshVar "len" t
      compileProg (Just lenvar) a
      return lenvar

mkBranch :: Location -> Ut.UntypedFeld -> Ut.UntypedFeld
         -> Maybe (Ut.UntypedFeld) -> CodeWriter ()
mkBranch loc c th el = do
    ce <- compileExpr c
    (_, tb) <- confiscateBlock $ compileProg loc th
    (_, eb) <- if isJust el
                  then confiscateBlock $ compileProg loc (fromJust el)
                  else return (undefined, toBlock Empty)
    tellProg [Switch ce [(Pat (litB True), tb), (Pat (litB False), eb)]]

compileProg :: Location -> Ut.UntypedFeld -> CodeWriter ()
-- Array
compileProg loc (In (Ut.Parallel len (In (Ut.Lambda (Ut.Var v ta) ixf)))) = do
   let ix = mkVar (compileTypeRep ta) v
   len' <- mkLength len ta
   (_, b) <- confiscateBlock $ compileProg (ArrayElem <$> loc <*> pure ix) ixf
   tellProg [initArray loc len']
   tellProg [for True (lName ix) len' (litI32 1) b]
compileProg loc (In (Ut.Sequential len init' (In (Ut.Lambda (Ut.Var v tix) lt1))))
   | (bs1, In (Ut.Lambda (Ut.Var s tst) l))         <- collectLetBinders lt1
   , (bs, In (Ut.Tup2 (In (Ut.Variable t1)) (In (Ut.Variable t2)))) <- collectLetBinders l
   , not $ null bs
   , (e, step) <- last bs
   , t1 == e
   , t2 == e
   = do
        blocks <- mapM (confiscateBlock . compileBind) (bs1 ++ init bs)
        let (dss, lets) = unzip $ map (\(_, Block ds (Sequence body)) -> (ds, body)) blocks
        let ix = mkVar (compileTypeRep tix) v
        len' <- mkLength len tix
        st1 <- freshVar "st" tst
        let st = mkRef (compileTypeRep tst) s
            st_val = Deref st
        declareAlias st
        (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s st_val $ compileProg (ArrayElem <$> loc <*> pure ix) step
        withAlias s st_val $ compileProg (Just st1) init'
        tellProg [ Assign st (AddrOf st1)
                 , initArray loc len']
        tellProg [toProg $ Block (concat dss ++ ds) $
                  for False (lName ix) len' (litI32 1) $
                               toBlock $ Sequence (concat lets ++ body ++ maybe [] (\arr -> [Assign st $ AddrOf (ArrayElem arr ix)]) loc)]
compileProg loc (In (Ut.Sequential len st (In (Ut.Lambda (Ut.Var v t) lt1))))
  | (bs1, In (Ut.Lambda (Ut.Var s _) step))          <- collectLetBinders lt1
  = do
       blocks <- mapM (confiscateBlock . compileBind) bs1
       let tr' = typeof step
       let ix = mkVar (compileTypeRep t) v
           (dss, lets) = unzip $ map (\(_, Block ds (Sequence body)) -> (ds, body)) blocks
       len' <- mkLength len t
       tmp  <- freshVar "seq" tr'
       (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s (StructField tmp "member2") $ compileProg (Just tmp) step
       tellProg [initArray loc len']
       compileProg (Just $ StructField tmp "member2") st
       tellProg [toProg $ Block (concat dss ++ ds) $
                 for False (lName ix) len' (litI32 1) $
                               toBlock $ Sequence (concat lets ++ body ++
                                    [copyProg (ArrayElem <$> loc <*> pure ix) [StructField tmp "member1"]
                                    ])]
compileProg loc (In (Ut.Append a b)) = do
   a' <- compileExpr a
   b' <- compileExpr b
   tellProg [copyProg loc [a', b']]
compileProg loc (In (Ut.SetIx arr i a)) = do
   compileProg loc arr
   i' <- compileExpr i
   compileProg (ArrayElem <$> loc <*> pure i') a
compileProg (Just loc) (In (Ut.GetIx arr i)) = do
   a' <- compileExpr arr
   i' <- compileExpr i
   let el = ArrayElem a' i'
   tellProg $ if isArray $ typeof el
                then [Assign loc el]
                else [copyProg (Just loc) [el]]
compileProg loc (In (Ut.SetLength len arr)) = do
   len' <- compileExpr len
   compileProg loc arr
   tellProg [setLength loc len']
-- Binding
compileProg _   (In Ut.Lambda{}) = error "Can only compile top-level lambda"
compileProg loc (In (Ut.Let a (In (Ut.Lambda (Ut.Var v ta) body)))) = do
   e <- compileLet a ta v
   withAlias v e $ compileProg loc body
-- Bits
-- Complex
-- Condition
compileProg loc (In (Ut.Condition cond tHEN eLSE)) =
   mkBranch loc cond tHEN $ Just eLSE
compileProg loc (In (Ut.ConditionM cond tHEN eLSE)) =
   mkBranch loc cond tHEN $ Just eLSE
-- Conversion
-- Elements
compileProg loc (In (Ut.EMaterialize len arr)) = do
   len' <- mkLength len (typeof len)
   tellProg [initArray loc len']
   compileProg loc arr
compileProg (Just loc) (In (Ut.EWrite ix e)) = do
   dst <- compileExpr ix
   compileProg (Just $ ArrayElem loc dst) e
compileProg loc (In Ut.ESkip) = tellProg [Comment True "Skip"]
compileProg loc (In (Ut.EPar p1 p2)) = do
   (_, Block ds1 b1) <- confiscateBlock $ compileProg loc p1
   (_, Block ds2 b2) <- confiscateBlock $ compileProg loc p2
   tellProg [toProg $ Block (ds1 ++ ds2) (Sequence [b1,b2])]
compileProg loc (In (Ut.EparFor len (In (Ut.Lambda (Ut.Var v ta) ixf)))) = do
   let ix = mkVar (compileTypeRep ta) v
   len' <- mkLength len ta
   (_, ixf') <- confiscateBlock $ compileProg loc ixf
   tellProg [for True (lName ix) len' (litI32 1) ixf']
-- Error
compileProg loc (In Ut.Undefined) = return ()
compileProg loc (In (Ut.Assert cond a)) = do
   compileAssert cond "temprary msg" -- msg
   compileProg loc a
-- Future
compileProg (Just loc) (In (Ut.MkFuture p)) = do
   env <- ask
   let args = nub $ [case lookup v (alias env) of
                    Nothing -> mkVariable (compileTypeRep t) v
                    Just (VarExpr e) -> e
                    -- Variables that got a Pointer wrapped around
                    -- their type in FromCore.
                    Just (Deref (VarExpr e)) -> e
              | (Ut.Var v t) <- Ut.fv p
              ] ++ fv loc
   -- Task core:
   ((_, ws), Block ds bl)  <- confiscateBigBlock $ do
       p' <- compileExprVar p
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
compileProg loc (In (Ut.Await a)) = do
   fut <- compileExprVar a -- compileExpr a
   tellProg [iVarGet l fut | Just l <- [loc]]
-- Literal
compileProg loc (In (Ut.Literal a)) = case loc of
     Just l -> literalLoc l a
     Nothing -> return ()
-- Logic
-- Loop
compileProg (Just loc) (In (Ut.ForLoop len init (In (Ut.Lambda (Ut.Var ix ta) lt1))))
  | (bs1, In (Ut.Lambda (Ut.Var st stt) ixf)) <- collectLetBinders lt1
  = do
      blocks <- mapM (confiscateBlock . compileBind) bs1
      let (dss, lets) = unzip $ map (\(_, Block ds (Sequence body)) -> (ds, body)) blocks
      let ix' = mkVar (compileTypeRep ta) ix
          stvar = mkVar (compileTypeRep (typeof ixf)) st
      len' <- mkLength len ta
      -- Large results, such as arrays, should be used in-place to avoid
      -- copying. Small results that fit in a register should be copied.
      --
      -- Makes a noticeable difference for the innermost loop in matrix
      -- multiplication in MultiDim.
      let (st', alias) | isArray $ typeof stvar = (loc, withAlias st loc)
                       | otherwise = (stvar, id)
      compileProg (Just st') init
      (_, Block ds body) <- alias $ confiscateBlock $ compileProg (Just stvar) ixf >> assign (Just st') stvar
      declare stvar
      tellProg [toProg $ Block (concat dss ++ ds) (for False (lName ix') len' (litI32 1) (toBlock $ Sequence $ concat lets ++ [body]))]
      tellProg [copyProg (Just loc) [st']]
compileProg (Just loc) (In (Ut.WhileLoop init (In (Ut.Lambda (Ut.Var cv ct) cond)) (In (Ut.Lambda (Ut.Var bv bt) body)))) = do
    let stvar = mkVar (compileTypeRep bt) bv
        condv = mkVar (compileTypeRep (typeof cond)) cv
    compileProg (Just loc) init >> assign (Just stvar) loc
    (_, cond') <- confiscateBlock $ withAlias cv loc $ compileProg (Just condv) cond
    (_, body') <- withAlias bv loc $ confiscateBlock $ compileProg (Just stvar) body >> assign (Just loc) stvar
    declare stvar
    declare condv
    tellProg [while cond' condv body']
-- LoopM
compileProg loc (In (Ut.While (In (Ut.Lambda v cond)) step)) = do
   condv <- freshVar "cond" (typeof cond)
   (_, cond') <- confiscateBlock $ compileProg (Just condv) cond
   (_, step') <- confiscateBlock $ compileProg loc step
   tellProg [while cond' condv step']
compileProg loc (In (Ut.For len (In (Ut.Lambda (Ut.Var v ta) ixf)))) = do
   let ix = mkVar (compileTypeRep ta) v
   len' <- mkLength len ta
   (_, Block ds body) <- confiscateBlock $ compileProg loc ixf
   tellProg [toProg $ Block ds (for False (lName ix) len' (litI32 1) (toBlock body))]
-- Mutable
compileProg loc (In (Ut.Run ma)) = compileProg loc ma
compileProg loc (In (Ut.Return a))
  | Ut.MutType Ut.UnitType <- Ut.typeof a = return ()
  | Ut.ParType Ut.UnitType <- Ut.typeof a = return ()
  | otherwise = compileProg loc a
compileProg loc (In (Ut.Bind ma (In (Ut.Lambda (Ut.Var v ta) body))))
  | (In Ut.ParNew) <- ma = do
   let var = mkVar (compileTypeRep ta) v
   declare var
   tellProg [iVarInit (AddrOf var)]
   compileProg loc body
  | otherwise = do
   let var  = mkVar (compileTypeRep ta) v
   declare var
   compileProg (Just var) ma
   compileProg loc body
compileProg loc (In (Ut.Then ma mb)) = do
   compileProg Nothing ma
   compileProg loc mb
compileProg loc (In (Ut.When c action)) =
   mkBranch loc c action Nothing
-- MutableArray
compileProg loc (In (Ut.NewArr len a)) = do
   nId <- freshId
   let ix = varToExpr $ mkNamedVar "i" (Rep.MachineVector 1 (Rep.NumType Ut.Unsigned Ut.S32)) nId
   a' <- compileExpr a
   l  <- compileExpr len
   tellProg [initArray loc l]
   tellProg [for False "i" l (litI32 1) $ toBlock (Sequence [copyProg (ArrayElem <$> loc <*> pure ix) [a']])]
compileProg loc (In (Ut.NewArr_ len)) = do
   l <- compileExpr len
   tellProg [initArray loc l]
compileProg loc (In (Ut.GetArr arr i)) = do
   arr' <- compileExpr arr
   i'   <- compileExpr i
   assign loc (ArrayElem arr' i')
compileProg loc (In (Ut.SetArr arr i a)) = do
   arr' <- compileExpr arr
   i'   <- compileExpr i
   a'   <- compileExpr a
   assign (Just $ ArrayElem arr' i') a'
-- MutableReference
compileProg loc (In (Ut.NewRef a)) = compileProg loc a
compileProg loc (In (Ut.GetRef r)) = compileProg loc r
compileProg loc (In (Ut.SetRef r a)) = do
   var  <- compileExpr r
   compileProg (Just var) a
compileProg loc (In (Ut.ModRef r (In (Ut.Lambda (Ut.Var v ta) body)))) = do
   var <- compileExpr r
   withAlias v var $ compileProg (Just var) body
       -- Since the modifier function is pure it is safe to alias
       -- v with var here
-- MutableToPure
compileProg (Just loc) (In (Ut.RunMutableArray marr))
 | (In (Ut.Bind (In (Ut.NewArr_ l)) (In (Ut.Lambda (Ut.Var v t) body)))) <- marr
 , (In (Ut.Return (In (Ut.Variable (Ut.Var r _))))) <- chaseBind body
 , v == r
 = do
     len <- compileExpr l
     tellProg [setLength (Just loc) len]
     withAlias v loc $ compileProg (Just loc) body
compileProg loc (In (Ut.RunMutableArray marr)) = compileProg loc marr
compileProg loc (In (Ut.WithArray marr@(In Ut.Variable{}) (In (Ut.Lambda (Ut.Var v ta) body)))) = do
    e <- compileExpr marr
    withAlias v e $ do
      b <- compileExpr body
      tellProg [copyProg loc [b]]
compileProg loc (In (Ut.WithArray marr (In (Ut.Lambda (Ut.Var v ta) body)))) = do
    let var = mkVar (compileTypeRep ta) v
    declare var
    compileProg (Just var) marr
    e <- compileExpr body
    tellProg [copyProg loc [e]]


-- Noinline
compileProg (Just loc) (In (Ut.NoInline p)) = do
    let args = nub $ [mkVariable (compileTypeRep t) v
               | (Ut.Var v t) <- Ut.fv p
               ] ++ fv loc
    (_, b)  <- confiscateBlock $ compileProg (Just loc) p
    let isInParam v = vName v /= lName loc
    let (ins,outs) = partition isInParam args
    funId  <- freshId
    let funname = "noinline" ++ show funId
    tellDef [Proc funname ins outs $ Just b]
    let ins' = map (\v -> ValueParameter $ varToExpr $ Rep.Variable (typeof v) (vName v)) ins
    tellProg [call funname $ ins' ++ [ValueParameter loc]]
-- Par
compileProg loc (In (Ut.ParRun p)) = compileProg loc p
compileProg loc (In (Ut.ParNew)) = return ()
compileProg loc (In (Ut.ParGet r)) = do
    iv <- compileExpr r
    tellProg [iVarGet l iv | Just l <- [loc]]
compileProg loc (In (Ut.ParPut r a)) = do
    iv  <- compileExpr r
    val <- compileExpr a
    i   <- freshId
    let var = varToExpr $ mkNamedVar "msg" (typeof val) i
    declare var
    assign (Just var) val
    tellProg [iVarPut iv var]
compileProg loc (In (Ut.ParFork p)) = do
   env <- ask
   let args = nub $ [case lookup v (alias env) of
                     Nothing -> mkVariable (compileTypeRep t) v
                     Just (VarExpr e) -> e
                     Just (Deref (VarExpr e)) -> e
               | (Ut.Var v t) <- Ut.fv p
               ] ++ maybe [] fv loc
   -- Task core:
   ((_, ws), Block ds b) <- confiscateBigBlock $ compileProg loc p
   funId  <- freshId
   let coreName = "task_core" ++ show funId
   tellDef [Proc coreName args [] $ Just (Block (decl ws ++ ds) b)]
   -- Task:
   let taskName = "task" ++ show funId
       runTask = Just $ toBlock $ run coreName args
   tellDef [Proc taskName [] [mkNamedRef "params" Rep.VoidType (-1)] runTask]
   -- Spawn:
   tellProg [spawn taskName args]
compileProg loc (In (Ut.ParYield)) = return ()
-- Save
compileProg loc (In (Ut.Save e)) = compileProg loc e
-- SizeProp
compileProg loc (In (Ut.PropSize e)) = compileProg loc e
-- SourceInfo
compileProg loc (In (Ut.SourceInfo a)) = do
    -- tellProg [Comment True info] XXX: Add this when we have annotations.
    compileProg loc a
-- Switch
compileProg loc (In (Ut.Switch tree@(In (Ut.Condition (In (Ut.Equal _ s)) _ _)))) = do
    scrutinee <- compileExpr s
    alts      <- chaseTree loc s tree
    tellProg [Switch{..}]
compileProg loc (In (Ut.Switch tree)) = compileProg loc tree
-- Tuple
compileProg loc (In (Ut.Tup2 m1 m2)) = do
    compileProg (StructField <$> loc <*> pure "member1") m1
    compileProg (StructField <$> loc <*> pure "member2") m2
compileProg loc (In (Ut.Tup3 m1 m2 m3)) = do
    compileProg (StructField <$> loc <*> pure "member1") m1
    compileProg (StructField <$> loc <*> pure "member2") m2
    compileProg (StructField <$> loc <*> pure "member3") m3
compileProg loc (In (Ut.Tup4 m1 m2 m3 m4)) = do
    compileProg (StructField <$> loc <*> pure "member1") m1
    compileProg (StructField <$> loc <*> pure "member2") m2
    compileProg (StructField <$> loc <*> pure "member3") m3
    compileProg (StructField <$> loc <*> pure "member4") m4
compileProg loc (In (Ut.Tup5 m1 m2 m3 m4 m5)) = do
    compileProg (StructField <$> loc <*> pure "member1") m1
    compileProg (StructField <$> loc <*> pure "member2") m2
    compileProg (StructField <$> loc <*> pure "member3") m3
    compileProg (StructField <$> loc <*> pure "member4") m4
    compileProg (StructField <$> loc <*> pure "member5") m5
compileProg loc (In (Ut.Tup6 m1 m2 m3 m4 m5 m6)) = do
    compileProg (StructField <$> loc <*> pure "member1") m1
    compileProg (StructField <$> loc <*> pure "member2") m2
    compileProg (StructField <$> loc <*> pure "member3") m3
    compileProg (StructField <$> loc <*> pure "member4") m4
    compileProg (StructField <$> loc <*> pure "member5") m5
    compileProg (StructField <$> loc <*> pure "member6") m6
compileProg loc (In (Ut.Tup7 m1 m2 m3 m4 m5 m6 m7)) = do
    compileProg (StructField <$> loc <*> pure "member1") m1
    compileProg (StructField <$> loc <*> pure "member2") m2
    compileProg (StructField <$> loc <*> pure "member3") m3
    compileProg (StructField <$> loc <*> pure "member4") m4
    compileProg (StructField <$> loc <*> pure "member5") m5
    compileProg (StructField <$> loc <*> pure "member6") m6
    compileProg (StructField <$> loc <*> pure "member7") m7
compileProg loc e = compileExprLoc loc e


compileExpr :: Ut.UntypedFeld -> CodeWriter (Expression ())
-- Array
compileExpr (In (Ut.GetLength a)) = do
   aExpr <- compileExpr a
   return $ arrayLength aExpr
compileExpr (In (Ut.GetIx arr i)) = do
   a' <- compileExpr arr
   i' <- compileExpr i
   return $ ArrayElem a' i'
-- Binding
compileExpr (In (Ut.Variable (Ut.Var v t))) = do
        env <- ask
        case lookup v (alias env) of
          Nothing -> return $ mkVar (compileTypeRep t) v
          Just e  -> return e
compileExpr (In (Ut.Let a (In (Ut.Lambda (Ut.Var v ta) body)))) = do
    e <- compileLet a ta v
    withAlias v e $ compileExpr body
-- Bits
compileExpr (In (Ut.BAnd e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Infix (typeof e1') True "&" [e1', e2']
compileExpr (In (Ut.BOr e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Infix (typeof e1') True "|" [e1', e2']
compileExpr (In (Ut.BXor e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "xor" [e1', e2']
compileExpr (In (Ut.Complement e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "complement" [e']
compileExpr (In (Ut.Bit e)) = do
    e' <- compileExpr e
    return $ fun' Prefix Rep.VoidType True "bit" [e']
compileExpr (In (Ut.SetBit e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "setBit" [e1', e2']
compileExpr (In (Ut.ClearBit e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "" [e1', e2']
compileExpr (In (Ut.ComplementBit e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "complementBit" [e1', e2']
compileExpr (In (Ut.TestBit e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (Rep.MachineVector 1 Rep.BoolType) True "testBit" [e1', e2']
compileExpr (In (Ut.ShiftLU e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "shiftLU" [e1', e2']
compileExpr (In (Ut.ShiftRU e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "shiftRU" [e1', e2']
compileExpr (In (Ut.ShiftL e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "shiftL" [e1', e2']
compileExpr (In (Ut.ShiftR e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "shiftR" [e1', e2']
compileExpr (In (Ut.RotateLU e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "rotateLU" [e1', e2']
compileExpr (In (Ut.RotateRU e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "rotateRU" [e1', e2']
compileExpr (In (Ut.RotateL e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "rotateL" [e1', e2']
compileExpr (In (Ut.RotateR e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "rotateR" [e1', e2']
compileExpr (In (Ut.ReverseBits e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "reverseBits" [e']
compileExpr (In (Ut.BitScan e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (Rep.MachineVector 1 (Rep.NumType Ut.Unsigned Ut.S32)) True "bitScan" [e']
compileExpr (In (Ut.BitCount e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (Rep.MachineVector 1 (Rep.NumType Ut.Unsigned Ut.S32)) True "bitCount" [e']
-- Complex
compileExpr (In (Ut.MkComplex e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (Rep.MachineVector 1 (Rep.ComplexType (typeof e1'))) True "complex" [e1', e2']
compileExpr (In (Ut.RealPart e)) = do
    e' <- compileExpr e
    let (Rep.MachineVector 1 (Rep.ComplexType t)) = typeof e'
    return $ fun' Prefix t True "creal" [e']
compileExpr (In (Ut.ImagPart e)) = do
    e' <- compileExpr e
    let (Rep.MachineVector 1 (Rep.ComplexType t)) = typeof e'
    return $ fun' Prefix t True "cimag" [e']
compileExpr (In (Ut.Conjugate e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "conjugate" [e']
compileExpr (In (Ut.MkPolar e1 e2))= do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (Rep.MachineVector 1 (Rep.ComplexType (typeof e1'))) True "complex" [e1', e2']
compileExpr (In (Ut.Magnitude e)) = do
    e' <- compileExpr e
    let (Rep.MachineVector 1 (Rep.ComplexType t)) = typeof e'
    return $ fun' Prefix t True "magnitude" [e']
compileExpr (In (Ut.Phase e)) = do
    e' <- compileExpr e
    let (Rep.MachineVector 1 (Rep.ComplexType t)) = typeof e'
    return $ fun' Prefix t True "phase" [e']
compileExpr (In (Ut.Cis e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (Rep.MachineVector 1 (Rep.ComplexType (typeof e'))) True "cis" [e']
-- Conversion
compileExpr (In (Ut.F2I t e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (compileTypeRep t) True "f2i" [e']
compileExpr (In (Ut.I2N t e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (compileTypeRep t) True "i2n" [e']
compileExpr (In (Ut.B2I t e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (compileTypeRep t) True "b2i" [e']
compileExpr (In (Ut.Round t e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (compileTypeRep t) True "round" [e']
compileExpr (In (Ut.Ceiling t e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (compileTypeRep t) True "ceiling" [e']
compileExpr (In (Ut.Floor t e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (compileTypeRep t) True "floor" [e']
-- Error
compileExpr (In (Ut.Assert cond a)) = do
    compileAssert cond "temp msg" -- msg
    compileExpr a
-- Eq
compileExpr (In (Ut.Equal e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Infix (Rep.MachineVector 1 Rep.BoolType) True "==" [e1', e2']
compileExpr (In (Ut.NotEqual e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Infix (Rep.MachineVector 1 Rep.BoolType) True "/=" [e1', e2']
-- FFI
compileExpr (In (Ut.ForeignImport name es)) = do
    es' <- mapM compileExpr es
    return $ fun' Prefix Rep.VoidType True name es'
-- Fractional
compileExpr (In (Ut.DivFrac e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "div" [e1', e2']
-- Floating
compileExpr (In Ut.Pi) = error "No pi ready"
compileExpr (In (Ut.Exp e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "exp" [e']
compileExpr (In (Ut.Sqrt e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "sqrt" [e']
compileExpr (In (Ut.Log e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "log" [e']
compileExpr (In (Ut.Pow e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "pow" [e1', e2']
compileExpr (In (Ut.LogBase e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "logBase" [e1', e2']
compileExpr (In (Ut.Sin e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "sin" [e']
compileExpr (In (Ut.Tan e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "tan" [e']
compileExpr (In (Ut.Cos e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "cos" [e']
compileExpr (In (Ut.Asin e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "asin" [e']
compileExpr (In (Ut.Atan e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "atan" [e']
compileExpr (In (Ut.Acos e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "acos" [e']
compileExpr (In (Ut.Sinh e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "sinh" [e']
compileExpr (In (Ut.Tanh e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "tanh" [e']
compileExpr (In (Ut.Cosh e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "cosh" [e']
compileExpr (In (Ut.Asinh e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "asinh" [e']
compileExpr (In (Ut.Atanh e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "atanh" [e']
compileExpr (In (Ut.Acosh e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (typeof e') True "acosh" [e']
-- Integral
compileExpr (In (Ut.Quot e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "quot" [e1', e2']
compileExpr (In (Ut.Rem e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "rem" [e1', e2']
compileExpr (In (Ut.Div e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "div" [e1', e2']
compileExpr (In (Ut.Mod e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "mod" [e1', e2']
compileExpr (In (Ut.IExp e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "exp" [e1', e2']
-- Literal
compileExpr (In (Ut.Literal l)) = literal l
-- Logic
compileExpr (In (Ut.And e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Infix (Rep.MachineVector 1 Rep.BoolType) True "&&" [e1', e2']
compileExpr (In (Ut.Or e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Infix (Rep.MachineVector 1 Rep.BoolType) True "||" [e1', e2']
compileExpr (In (Ut.Not e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (Rep.MachineVector 1 Rep.BoolType) True "not" [e']
-- Mutable
compileExpr (In (Ut.Run ma)) = compileExpr ma
-- MutableArray
compileExpr (In (Ut.ArrLength arr)) = do
    a' <- compileExpr arr
    return $ arrayLength a'
-- MutableReference
compileExpr (In (Ut.GetRef r)) = compileExpr r
-- Num
compileExpr (In (Ut.Abs e))  = do
    e' <- compileExpr e
    return $ fun' Prefix (Rep.MachineVector 1 Rep.BoolType) True "abs" [e']
compileExpr (In (Ut.Sign e)) = do
    e' <- compileExpr e
    return $ fun' Prefix (Rep.MachineVector 1 Rep.BoolType) True "signum" [e']
compileExpr (In (Ut.Add e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Infix (typeof e1') True "+" [e1', e2']
compileExpr (In (Ut.Sub e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Infix (typeof e1') True "-" [e1', e2']
compileExpr (In (Ut.Mul e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Infix (typeof e1') True "*" [e1', e2']
-- Ord
compileExpr (In (Ut.LTH e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Infix (Rep.MachineVector 1 Rep.BoolType) True "<" [e1', e2']
compileExpr (In (Ut.GTH e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Infix (Rep.MachineVector 1 Rep.BoolType) True ">" [e1', e2']
compileExpr (In (Ut.LTE e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Infix (Rep.MachineVector 1 Rep.BoolType) True "<=" [e1', e2']
compileExpr (In (Ut.GTE e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Infix (Rep.MachineVector 1 Rep.BoolType) True ">=" [e1', e2']
compileExpr (In (Ut.Min e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "min" [e1', e2']
compileExpr (In (Ut.Max e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "max" [e1', e2']
-- SizeProp
compileExpr (In (Ut.PropSize e)) = compileExpr e
-- RealFloat
compileExpr (In (Ut.Atan2 e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e1') True "atan2" [e1', e2']
-- SourceInfo
compileExpr (In (Ut.SourceInfo a)) = do
    -- tellProg [Comment True info] XXX: Add this when we have annotations.
    compileExpr a
-- Trace
compileExpr (In (Ut.Trace e1 e2)) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    return $ fun' Prefix (typeof e2') True "trace" [e1', e2']
-- Tuple
compileExpr (In (Ut.Sel1 tup)) = do
    tupExpr <- compileExpr tup
    return $ StructField tupExpr "member1"
compileExpr (In (Ut.Sel2 tup)) = do
    tupExpr <- compileExpr tup
    return $ StructField tupExpr "member2"
compileExpr (In (Ut.Sel3 tup)) = do
    tupExpr <- compileExpr tup
    return $ StructField tupExpr "member3"
compileExpr (In (Ut.Sel4 tup)) = do
    tupExpr <- compileExpr tup
    return $ StructField tupExpr "member4"
compileExpr (In (Ut.Sel5 tup)) = do
    tupExpr <- compileExpr tup
    return $ StructField tupExpr "member5"
compileExpr (In (Ut.Sel6 tup)) = do
    tupExpr <- compileExpr tup
    return $ StructField tupExpr "member6"
compileExpr (In (Ut.Sel7 tup)) = do
    tupExpr <- compileExpr tup
    return $ StructField tupExpr "member7"
compileExpr e = compileProgFresh e

compileLet :: Ut.UntypedFeld -> Ut.Type -> Integer -> CodeWriter (Expression ())
compileLet a ta v = do
   let var = mkVar (compileTypeRep ta) v
   declare var
   compileProg (Just var) a
   return var

compileAssert :: Ut.UntypedFeld -> String -> CodeWriter ()
compileAssert cond msg = do
    condExpr <- compileExpr cond
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

chaseTree :: Location -> Ut.UntypedFeld -> Ut.UntypedFeld
            -> CodeWriter [(Pattern (), Block ())]
chaseTree loc _s (In (Ut.Condition (In (Ut.Equal c  a)) t f))
    -- , alphaEq s a -- TODO check that the scrutinees are equal
    = do
         e <- compileExpr c
         (_,body) <- confiscateBlock $ compileProg loc t
         cases <- chaseTree loc _s f
         return $ (Pat e, body) : cases

chaseTree loc _ a = do
    (_,body) <- confiscateBlock $ compileProg loc a
    return [(PatDefault, body)]

-- | Chase down the right-spine of `Bind` and `Then` constructs and return
-- the last term
chaseBind :: Ut.UntypedFeld -> Ut.UntypedFeld
chaseBind (In (Ut.Bind _ (In (Ut.Lambda _  body)))) = chaseBind body
chaseBind (In (Ut.Then _ body))                     = chaseBind body
chaseBind a                                         = a

{- NOTES:

The trick of doing a copy at the end, i.e. `tellProg [copyProg loc
e]`, when compiling WithArray is important. It allows us to safely
return the pure array that is passed in as input. This is nice because
it allows us to implement `freezeArray` in terms of `withArray`.
In most cases I expect `withArray` to return a scalar as its final
result and then the copyProg is harmless.
-}

collectLetBinders :: Ut.UntypedFeld -> ([(Ut.Var, Ut.UntypedFeld)], Ut.UntypedFeld)
collectLetBinders e = go e []
  where go (In (Ut.Let e (In (Ut.Lambda v b)))) acc = go b ((v, e):acc)
        go e acc = (reverse acc, e)

compileBind :: (Ut.Var, Ut.UntypedFeld) -> CodeWriter ()
compileBind (Ut.Var v t, e) = do
   let var = mkVar (compileTypeRep t) v
   declare var
   compileProg (Just var) e
