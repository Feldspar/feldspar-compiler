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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Backend.C.Plugin.PrettyPrint where

import Feldspar.Transformation
import Feldspar.Compiler.Backend.C.CodeGeneration
import Feldspar.Compiler.Backend.C.Options

import Feldspar.Range

import qualified Data.List as List (find, intercalate)
import qualified Control.Monad.State as StateMonad (get, put, runState)

-- ===========================================================================
--  == DebugToC plugin
-- ===========================================================================

data DebugToC = DebugToC

data DebugToCSemanticInfo

instance Annotation DebugToCSemanticInfo Module where
    type Label DebugToCSemanticInfo Module = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Entity where
    type Label DebugToCSemanticInfo Entity = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Struct where
    type Label DebugToCSemanticInfo Struct = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo ProcDef where
    type Label DebugToCSemanticInfo ProcDef = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo ProcDecl where
    type Label DebugToCSemanticInfo ProcDecl = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo StructMember where
    type Label DebugToCSemanticInfo StructMember = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Block where
    type Label DebugToCSemanticInfo Block = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Program where
    type Label DebugToCSemanticInfo Program = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Empty where
    type Label DebugToCSemanticInfo Empty = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Assign where
    type Label DebugToCSemanticInfo Assign = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo ProcedureCall where
    type Label DebugToCSemanticInfo ProcedureCall = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Spawn where
    type Label DebugToCSemanticInfo Spawn = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Run where
    type Label DebugToCSemanticInfo Run = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Sequence where
    type Label DebugToCSemanticInfo Sequence = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Branch where
    type Label DebugToCSemanticInfo Branch = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Switch where
    type Label DebugToCSemanticInfo Switch = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo SeqLoop where
    type Label DebugToCSemanticInfo SeqLoop = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo ParLoop where
    type Label DebugToCSemanticInfo ParLoop = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo ActualParameter where
    type Label DebugToCSemanticInfo ActualParameter = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Declaration where
    type Label DebugToCSemanticInfo Declaration = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Expression where
    type Label DebugToCSemanticInfo Expression = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo FunctionCall where
    type Label DebugToCSemanticInfo FunctionCall = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo SizeOf where
    type Label DebugToCSemanticInfo SizeOf = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo ArrayElem where
    type Label DebugToCSemanticInfo ArrayElem = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo StructField where
    type Label DebugToCSemanticInfo StructField = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Constant where
    type Label DebugToCSemanticInfo Constant = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo IntConst where
    type Label DebugToCSemanticInfo IntConst = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo FloatConst where
    type Label DebugToCSemanticInfo FloatConst = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo BoolConst where
    type Label DebugToCSemanticInfo BoolConst = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo ArrayConst where
    type Label DebugToCSemanticInfo ArrayConst = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo ComplexConst where
    type Label DebugToCSemanticInfo ComplexConst = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Variable where
    type Label DebugToCSemanticInfo Variable = ((Int, Int), (Int, Int))

instance Annotation DebugToCSemanticInfo Cast where
    type Label DebugToCSemanticInfo Cast = ((Int, Int), (Int, Int))
    
instance Annotation DebugToCSemanticInfo Comment where
    type Label DebugToCSemanticInfo Comment = ((Int, Int), (Int, Int))


instance Transformation DebugToC where
    type From DebugToC    = ()
    type To DebugToC      = DebugToCSemanticInfo
    type Down DebugToC    = (Options, Place, Int)  -- Platform, Place and Indentation
    type Up DebugToC      = String
    type State DebugToC   = (Int, Int)

instance Plugin DebugToC where
    type ExternalInfo DebugToC = ((Options, Place), Int)
    executePlugin DebugToC ((options, place), line) procedure =
        result $ transform DebugToC (line, 0) (options, place, 0) procedure

compToC :: ((Options, Place), Int) -> Module () -> (String, (Int, Int))
compToC ((options, place), line) procedure = (up res, state res) where
    res = transform DebugToC (line, 0) (options, place, 0) procedure

compToCWithInfos :: ((Options, Place), Int) -> Module () -> (Module DebugToCSemanticInfo, (String, (Int, Int)))
compToCWithInfos ((options, place), line) procedure = (result res, (up res, state res)) where
    res = transform DebugToC (line, 0) (options, place, 0) procedure

instance Transformable DebugToC Variable where
    transform _ pos (options, place, _) x@(Variable vname typ role _) = Result (Variable vname typ role newInf) (snd newInf) cRep
        where
            (newInf, (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                code $ toC options place x
                (_, nl, nc) <- StateMonad.get
                return (pos, (nl, nc))

instance Transformable1 DebugToC [] Constant where
    transform1 t pos down l = transform1' t pos down l ", "

instance Transformable DebugToC Constant where
    transform t pos down cnst@(IntConst c _ _ _) = transformConst pos down cnst (show c)

    transform t pos down cnst@(FloatConst c _ _) = transformConst pos down cnst (show c ++ "f")

    transform t pos down cnst@(BoolConst False _ _) = transformConst pos down cnst "0"

    transform t pos down cnst@(BoolConst True _ _) = transformConst pos down cnst "1"

    transform t pos down@(options, _, _) cnst@(ComplexConst real im _ _)
        = case List.find (\(t',_) -> t' == typeof cnst) $ values $ platform options of
            Just (_,f) -> 
                Result (ComplexConst (result newReal) (result newIm) newInf newInf) (snd newInf) cRep 
                    where
                        ((newReal, newIm, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                            nr <- complexTransform t down real
                            ni <- complexTransform t down im
                            code $ f cnst
                            (_, nl, nc) <- StateMonad.get
                            return (nr, ni, (pos,(nl,nc)))
            Nothing    -> 
                Result (ComplexConst (result newReal) (result newIm) newInf newInf) (snd newInf) cRep
                    where
                        ((newReal, newIm, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                            code "complex("
                            nr <- monadicTransform' t down real
                            code ","
                            ni <- monadicTransform' t down im
                            code ")"
                            (_, nl, nc) <- StateMonad.get
                            return (nr, ni, (pos,(nl,nc)))

instance Transformable DebugToC ActualParameter where
    transform t pos down act@(In (VarExpr (Variable _ StructType{} _ _) _) _) =
        transformActParam t pos down act AddressNeed_pl
    transform t pos down act@(In (VarExpr (Variable _ ArrayType{} _ _) _) _) =
        transformActParam t pos down act AddressNeed_pl
    transform t pos down act@In{}            = transformActParam t pos down act FunctionCallIn_pl
    transform t pos down act@Out{}           = transformActParam t pos down act AddressNeed_pl
    transform t pos down act@TypeParameter{} = transformActParam t pos down act MainParameter_pl
    transform t pos down act@FunParameter{}  = transformActParam t pos down act FunctionCallIn_pl

instance Transformable1 DebugToC [] Expression where
    transform1 t pos down l = transform1' t pos down l ", "

instance Transformable DebugToC Expression where
    transform t pos down (VarExpr val _) = Result (VarExpr (result newVal) newInf) (snd newInf) cRep
        where
            ((newVal, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                nv <- monadicTransform' t down val
                (_, nl, nc) <- StateMonad.get
                return (nv, (pos,(nl,nc)))

    transform t pos down@(options, place, _) e@(ArrayElem n index _ _) = Result (ArrayElem (result newName) (result newIndex) newInf newInf) (snd newInf) cRep 
        where
            ((newName, newIndex, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                let prefix = case (place, typeof e) of
                       (AddressNeed_pl, _) -> "&"
                       (_, ArrayType _ _)  -> "&" -- TODO the call site should set the place to AddressNeed_pl for Arrays
                       _                   -> ""
                code $ prefix ++ "at(" ++ showType options Value Declaration_pl (typeof e) NoRestrict ++ ","
                nn <- monadicTransform' t (newPlace down AddressNeed_pl) n
                code ","
                ni <- monadicTransform' t (newPlace down ValueNeed_pl) index
                code ")"
                (_, nl, nc) <- StateMonad.get
                return (nn, ni, (pos,(nl,nc)))

    transform t pos down expr@(StructField _ field _ _) = transformExpr pos down ('.' : field) ValueNeed_pl
      where
          transformExpr pos (options, place, indent) str paramType = Result (newExpr expr) (snd newInf) cRep
            where
                newExpr (StructField _ s _ _ ) = StructField (result newTarget) s newInf newInf
                getExpr (StructField e _ _ _ ) = e
                prefix = case (place, typeof expr) of
                       (AddressNeed_pl, _) -> "&"
                       (_, ArrayType _ _)  -> "&"
                       _                   -> ""
                ((newTarget, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                    code prefix
                    nt <- monadicTransform' t (options, paramType, indent)  (getExpr expr)
                    code str
                    (_, nl, nc) <- StateMonad.get
                    return (nt, (pos,(nl,nc)))


    transform t pos down (ConstExpr val _) = Result (ConstExpr (result newVal) newInf) (snd newInf) cRep
        where
            ((newVal, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                nv <- monadicTransform' t down val
                (_, nl, nc) <- StateMonad.get
                return (nv, (pos,(nl,nc)))

    transform t pos down fc@(FunctionCall f [_,_] _ _)
        | funName f == "!" = transformFuncCall t pos down fc "at(" "," ")"

    transform t pos down fc@(FunctionCall f [_,_] _ _)
        | funMode f == Infix = transformFuncCall t pos down fc "(" (" " ++ funName f ++ " ") ")"

    transform t pos (options, _, indent) (FunctionCall f paramlist _ _) =
                Result (FunctionCall f (result1 newParamlist) newInf newInf) (snd newInf) cRep 
        where
            ((newParamlist, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                code $ funName f ++ "("
                npl <- monadicListTransform' t (options, FunctionCallIn_pl, indent) paramlist
                code ")"
                (_, nl, nc) <- StateMonad.get
                return (npl, (pos,(nl,nc)))

    transform t pos down@(options, place, _) (Cast typ e _ _) =  Result (Cast typ (result newExp) newInf newInf) (snd newInf) cRep 
        where
            ((newExp, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                code $ concat ["((", toC options place typ, ")("]
                ne <- monadicTransform' t down e
                code "))"
                (_, nl, nc) <- StateMonad.get
                return (ne, (pos,(nl,nc)))

    transform _ pos (options, place, _) (SizeOf (Left typ) _ _) = Result (SizeOf (Left typ) newInf newInf) (snd newInf) cRep 
        where
            (newInf, (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                code ("sizeof(" ++ toC options place typ ++ ")")
                (_, nl, nc) <- StateMonad.get
                return (pos, (nl, nc))

    transform t pos down (SizeOf (Right e) _ _) = Result (SizeOf (Right (result newExp)) newInf newInf) (newLine, newCol) cRep 
        where
            ((newExp, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState pos) $ do
                code "sizeof"
                ne <- monadicTransform' t down e
                code ")"
                (_, nl, nc) <- StateMonad.get
                return (ne, (pos,(nl,nc)))

instance Transformable1 DebugToC [] Entity where
    transform1 t pos down l = transform1' t pos down l ""

instance Transformable DebugToC Module where
    transform t pos down (Module defList _) = Result (Module (result1 newDefList) newInf) (snd newInf) cRep 
        where
            ((newDefList, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                ndl <- monadicListTransform' t down defList
                (_, nl, nc) <- StateMonad.get
                return (ndl, (pos,(nl,nc)))

instance Transformable1 DebugToC [] Variable where
    transform1 t pos down l = transform1' t pos down l ", "

instance Transformable1 DebugToC [] StructMember where
    transform1 _ pos _ [] = Result1 [] pos ""
    transform1 t pos down (x:xs) = Result1 (result newX : result1 newXs) (state1 newXs) cRep where
        ((newX, newXs), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
            indenter down
            nx  <- monadicTransform' t down x
            nxs <- monadicListTransform' t down xs
            return (nx, nxs)


instance Transformable DebugToC Entity where
    transform t pos down@(_, _, indent) (StructDef n members _ _) = Result (StructDef n (result1 newMembers) newInf newInf) (snd newInf) cRep
        where
            ((newMembers, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                code $ n ++ " {\n"
                (crep, cl, cc) <- StateMonad.get
                StateMonad.put (crep, cl, cc + (trd $ addIndent down))
                nms <- monadicListTransform' t (addIndent down) members
                indenter down
                code "};\n"
                (crep, cl, _) <- StateMonad.get
                StateMonad.put (crep, cl, indent)
                return (nms, (pos,(cl,indent)))

    transform _ pos (options, place, indent) (TypeDef typ n _) = Result (TypeDef typ n newInf) (snd newInf) cRep
        where
            (newInf, (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                code $ unwords [ "typedef"
                               , showType options Value place typ NoRestrict
                               , n
                               ]
                code ";\n"
                (crep, cl, _) <- StateMonad.get
                StateMonad.put (crep, cl, indent)
                return (pos, (cl, indent))

    transform t pos down@(_, _, indent) (ProcDef n inp outp body _ _) =
      Result (ProcDef n (result1 newInParam) (result1 newOutParam) (result newBody) newInf newInf) (snd newInf) cRep
        where
            ((newInParam, newOutParam, newBody, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                indenter down
                code $ "void " ++ n ++ "("
                ninp <- monadicListTransform' t (newPlace down MainParameter_pl) inp
                let str
                        | null inp || null outp = ""
                        | otherwise             = ", "
                code str
                noutp <- monadicListTransform' t (newPlace down MainParameter_pl) outp
                code ")\n"
                indenter down
                code "{\n"
                (crep, al, _) <- StateMonad.get
                StateMonad.put (crep, al, trd $ addIndent down)
                nb <- monadicTransform' t (newPlace (addIndent down) Declaration_pl) body
                indenter down
                code "}\n"
                (_, nl, _) <- StateMonad.get
                return (ninp, noutp, nb, (pos,(nl,indent)))

    transform t pos down@(options, _, indent) (ProcDecl n inp outp _ _) =
      Result (ProcDecl n (result1 newInParam) (result1 newOutParam) newInf newInf) (snd newInf) cRep
        where
            ((newInParam, newOutParam, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                indenter down
                code $ "void " ++ n ++ "("
                ninp <- monadicListTransform' t (options, MainParameter_pl, indent) inp
                let str
                        | null inp || null outp = ""
                        | otherwise             = ", "
                code str
                noutp <- monadicListTransform' t (options, MainParameter_pl, indent) outp
                code ");\n"
                (_, nl, _) <- StateMonad.get
                return (ninp, noutp, (pos,(nl,indent)))

displayComment :: Int -> [String] -> String
displayComment indent = unlines . map (putIndent indent ++) . (["/*"] ++) . (++ [" */"]) . map (" * " ++)

displayMemInfo :: String -> [Type] -> String
displayMemInfo n []   = n ++ ": none"
displayMemInfo n info = n ++ ": " ++ List.intercalate ", " (map displayType info)

displayType :: Type -> String
displayType (Alias t _) = displayType t
displayType (ArrayType n (ArrayType m t)) | isSingleton n && isSingleton m =
    unwords [displayType t, concat ["array(", show (upperBound n), "x", show (upperBound m), ")"]]
displayType (ArrayType n t) | isSingleton n = unwords [displayType t, concat ["array(", show (upperBound n), ")"]]
displayType (ArrayType r t) = unwords [displayType t, "array"]
displayType (IVarType t) = unwords ["ivar(", displayType t, ")"]
displayType VoidType = "void"
displayType BoolType = "Boolean"
displayType BitType = "bit"
displayType FloatType = "float"
displayType (NumType signed size) = unwords [sg signed, sz size ++ "-bit", "integer"]
  where
    sg Signed   = "signed"
    sg Unsigned = "unsigned"
    sz S8  = "8"
    sz S16 = "16"
    sz S32 = "32"
    sz S40 = "40"
    sz S64 = "64"
displayType (ComplexType t) = unwords ["complex", displayType t]
displayType (UserType s)    = s
displayType (StructType _)  = "struct"

instance Transformable DebugToC StructMember where
    transform _ pos (options, place, _) dsm@(StructMember str typ _) = Result (StructMember str typ newInf) (snd newInf) cRep 
        where
            (newInf, (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                let t = case structMemberType dsm of
                     ArrayType{} -> showVariable options place Value (structMemberType dsm) (structMemberName dsm) ++ ";"
                     _           -> toC options place (structMemberType dsm) ++ " " ++ structMemberName dsm ++ ";"
                code $ t ++ "\n"
                (_, nl, nc) <- StateMonad.get
                return (pos, (nl, nc))

instance Transformable1 DebugToC [] Declaration where
    transform1 _ pos _ [] = Result1 [] pos ""
    transform1 t pos down@(_, _, indent) (x:xs) = Result1 (result newX : result1 newXs) (state1 newXs) ((putIndent indent ++ up newX ++ ";\n" ) ++ up1 newXs) where
        newX = transform t pos down x
        (line2, _) =  state newX
        newSt = (line2 + 1, indent) 
        newXs = transform1 t newSt down xs

instance Transformable DebugToC Block where
    transform t pos down@(_, _, indent) (Block locs body _) = Result (Block (result1 newLocs) (result newBody) newInf) (snd newInf) cRep
        where
            ((newLocs, newBody, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                nlocs <- monadicListTransform' t (newPlace down Declaration_pl) locs
                let str = case up1 newLocs of 
                     "" -> ""
                     _  -> "\n"
                code str
                nbody <- monadicTransform' t down body
                (_, nl, _) <- StateMonad.get
                return (nlocs, nbody, (pos,(nl,indent)))

instance Transformable DebugToC Declaration where
    transform t pos down (Declaration dv Nothing _) = Result (Declaration (result newDeclVar) Nothing newInf) (snd newInf) cRep
        where
            ((newDeclVar, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                ndv <- monadicTransform' t (newPlace down Declaration_pl) dv
                case varType dv of
                    (ArrayType _ _) -> code " = {0}"
                    _               -> code ""
                (_, nl, nc) <- StateMonad.get
                return (ndv, (pos,(nl,nc)))

    transform t pos down (Declaration dv (Just e) _) = Result (Declaration (result newDeclVar) (Just (result newExpr)) newInf) (snd newInf) cRep 
        where
            ((newDeclVar, newExpr, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                ndv <- monadicTransform' t (newPlace down Declaration_pl) dv
                code " = "
                ne <- monadicTransform' t (newPlace down ValueNeed_pl) e
                (_, nl, nc) <- StateMonad.get
                return (ndv, ne, (pos,(nl,nc)))

instance Transformable1 DebugToC [] ActualParameter where
    transform1 t pos down l = transform1' t pos down l ", "

instance Transformable1 DebugToC [] Program where
    transform1 t pos down l = transform1' t pos down l ""


instance Transformable DebugToC Program where
    transform _ pos _ (Empty _ _) = Result (Empty newInf newInf) pos "" where 
        newInf = (pos, pos)

    transform _ pos down (Comment True comment _ _) = Result (Comment True comment newInf newInf) (snd newInf) cRep 
        where
            (newInf, (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                indenter down
                code $ "/* " ++ comment ++ " */\n"
                (_, nl, nc) <- StateMonad.get
                return (pos, (nl, nc))

    transform _ pos down (Comment False comment _ _) = Result (Comment False comment newInf newInf) (snd newInf) cRep 
        where 
            (newInf, (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                indenter down
                code $ "// " ++ comment ++ "\n"
                (_, nl, nc) <- StateMonad.get
                return (pos, (nl, nc))

    transform t pos down@(_, _, indent) (Assign lh rh _ _) = Result (Assign (result newLhs) (result newRhs) newInf newInf) (snd newInf) cRep
        where
            ((newLhs, newRhs, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                indenter down
                nlhs <- monadicTransform' t (newPlace down ValueNeed_pl) lh
                code " = "
                nrhs <- monadicTransform' t (newPlace down ValueNeed_pl) rh
                code ";\n"
                (_, nl, _) <- StateMonad.get
                return (nlhs, nrhs, (pos,(nl,indent)))

    transform t pos down@(_, _, indent) (ProcedureCall n param _ _) = Result (ProcedureCall n (result1 newParam) newInf newInf) (snd newInf) cRep 
        where
            ((newParam, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                indenter down
                code $ n ++ "("
                np <- monadicListTransform' t down param
                code ");\n"
                (_, nl, _) <- StateMonad.get
                return (np, (pos,(nl,indent)))

    transform t pos down (Sequence prog _ _) = Result (Sequence (result1 newProg) newInf newInf) (snd newInf) cRep 
        where
            ((newProg, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                np <- monadicListTransform' t down prog
                return (np, (pos,state1 newProg))

    transform t pos down (Branch con tPrg ePrg _ _) = Result (Branch (result newCon) (result newTPrg) (result newEPrg) newInf newInf) (snd newInf) cRep 
        where 
            ((newCon, newTPrg, newEPrg, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                indenter down
                code "if("
                ncon <- monadicTransform' t (newPlace down ValueNeed_pl) con
                code ")\n" 
                indenter down
                code "{\n"
                ntPrg <- monadicTransform' t (addIndent down) tPrg
                indenter down
                code "}\n" 
                indenter down
                code "else\n" 
                indenter down
                code "{\n"
                nePrg <- monadicTransform' t (addIndent down) ePrg
                indenter down
                code "}\n"
                (_, nl, nc) <- StateMonad.get
                return (ncon, ntPrg, nePrg, (pos,(nl,nc)))

    transform t pos down (Switch scrut alts _ _) = error "TODO: PrettyPrint for switch"

    transform t pos down (SeqLoop con conPrg blockPrg _ _) = Result (SeqLoop (result newCon) (result newConPrg) (result newBlockPrg) newInf newInf) (snd newInf) cRep 
        where
            ((newCon, newConPrg, newBlockPrg, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                indenter down
                code "{\n"
                ncp <- monadicTransform' t (addIndent down) conPrg
                indenter $ addIndent down
                code "while("
                ncon <- monadicTransform' t (newPlace (addIndent down) ValueNeed_pl) con
                code ")\n" 
                indenter $ addIndent down
                code "{\n"
                nbp <- monadicTransform' t (addIndent $ addIndent down) blockPrg
                monadicTransform' t (addIndent $ addIndent down) (blockBody conPrg)
                indenter $ addIndent down
                code "}\n" 
                indenter down
                code "}\n"
                (_, nl, nc) <- StateMonad.get
                return (ncon, ncp, nbp, (pos,(nl,nc)))

    transform t pos down (ParLoop count bound step prog _ _) = Result (ParLoop (result newCount) (result newBound) step (result newProg) newInf newInf) (snd newInf) cRep 
        where
            ((newCount, newBound, newProg, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                indenter down
                code "for("
                _ <- monadicTransform' t (newPlace (addIndent down) Declaration_pl) count
                code " = 0; "
                loopVariable <- monadicTransform' t (newPlace (addIndent down) ValueNeed_pl) count
                code " < "
                nb <- monadicTransform' t (newPlace (addIndent down) ValueNeed_pl) bound
                code $ "; " ++ up loopVariable ++ " += " ++ show step ++ ")\n" 
                indenter down
                code "{\n"
                np <- monadicTransform' t (addIndent down) prog
                indenter down
                code "}\n" 
                (_, nl, nc) <- StateMonad.get
                return (loopVariable, nb, np, (pos,(nl,nc)))

    transform t pos down (BlockProgram prog _) = Result (BlockProgram (result newProg) newInf) (snd newInf) cRep 
        where
            ((newProg, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
                indenter down
                code "{\n"
                np <- monadicTransform' t (addIndent down) prog
                indenter down
                code "}\n"
                (_, nl, nc) <- StateMonad.get
                return (np, (pos,(nl,nc)))

putIndent :: Int -> String
putIndent = concat . flip replicate " "

addIndent :: (Options, Place, Int) -> (Options, Place, Int)
addIndent (options, place, indent) = (options, place, indent + 4)

transform1' _ pos _ [] _ = Result1 [] pos ""
transform1' t pos down (x:[]) _ = Result1 [result newX] (state newX) (up newX) where
    newX = transform t pos down x
transform1' t pos down (x:xs) str = Result1 (result newX : result1 newXs) (state1 newXs) (up newX ++ str ++ up1 newXs) where
    newX = transform t pos down x
    (line2, col2) =  state newX
    newSt = (line2, col2 + length str)
    newXs = transform1 t newSt down xs

transformConst pos@(line, _) (options, _, _) (cnst :: Constant ()) str = Result (newConst cnst) (line, newCol) cRep 
    where
        newConst (IntConst c t _ _) = IntConst c t newInf newInf
        newConst (FloatConst c _ _) = FloatConst c newInf newInf
        newConst (BoolConst c _ _)  = BoolConst c newInf newInf
        newInf = (pos, (line, newCol))
        (cRep, _, newCol) = snd $ flip StateMonad.runState (defaultState pos) $ do
        let s = case List.find (\(t',_) -> t' == typeof cnst) $ values $ platform options of
             Just (_,f) -> f cnst
             Nothing    -> str
        code s

transformActParam _ pos (options, _, _) (TypeParameter typ mode _) _ = Result newParam (snd newInf) cRep 
    where
        newParam = TypeParameter typ mode newInf
        place Auto = MainParameter_pl
        place Scalar = Declaration_pl
        (newInf, (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
            code $ showType options Value (place mode) typ NoRestrict
            (_, nl, nc) <- StateMonad.get
            return (pos,(nl,nc))

transformActParam _ pos _ (FunParameter n addr _) _ = Result newParam (snd newInf) cRep 
    where
        newParam = FunParameter n addr newInf
        (newInf, (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
            let addrOp
                    | addr      = "&"
                    | otherwise = ""
            code $ addrOp ++ n
            (_, nl, nc) <- StateMonad.get
            return (pos, (nl, nc))

transformActParam t pos down act paramType = Result (newActParam act) (snd newInf) cRep 
    where
        newActParam Out{} = Out (result newParam) newInf
        newActParam In{}  = In  (result newParam) newInf
        getParam (In param _)  = param
        getParam (Out param _) = param
        ((newParam, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
            np <- monadicTransform' t (newPlace down paramType) (getParam act)
            (_, nl, nc) <- StateMonad.get
            return (np, (pos,(nl,nc)))

transformFuncCall t pos down
                  (FunctionCall f [a, b] _ _) str1 str2 str3 =
                  Result (FunctionCall f [result newA, result newB] newInf newInf) (snd newInf) cRep
    where
        ((newA, newB, newInf), (cRep, _, _)) = flip StateMonad.runState (defaultState pos) $ do
            code str1
            na <- monadicTransform' t down a
            code str2
            nb <- monadicTransform' t down b
            code str3
            (_, nl, nc) <- StateMonad.get
            return (na, nb, (pos,(nl,nc)))

code s = do
    (str, line, col) <- StateMonad.get
    let numEOF = length (filter (=='\n') s)
    StateMonad.put (str++s,line + numEOF, (if numEOF == 0 then col else 0) + length (takeWhile (/='\n') $ reverse s))

indenter (_, _, n) = do
    (str, line, col) <- StateMonad.get
    StateMonad.put (str ++ concat (replicate n " "), line, col)

monadicTransform' t down d = do
    (_, line, col) <- StateMonad.get
    let res = transform t (line, col) down d
    code $ up res
    return res

complexTransform t down d = do
    (_, line, col) <- StateMonad.get
    return $ transform t (line, col) down d

monadicListTransform' t down l = do
    (_, line, col) <- StateMonad.get
    let resList = transform1 t (line, col) down l
    code $ up1 resList
    return resList

defaultState :: (Int, Int) -> (String, Int, Int)
defaultState (line, col) = ("", line, col)

trd :: (a, b, c) -> c
trd (_, _, e) = e

newPlace :: (Options, Place, Int) -> Place -> (Options, Place, Int)
newPlace (options, _, i) place = (options, place, i)
