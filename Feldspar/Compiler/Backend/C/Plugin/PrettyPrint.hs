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

module Feldspar.Compiler.Backend.C.Plugin.PrettyPrint where

import Feldspar.Transformation
import Feldspar.Compiler.Backend.C.CodeGeneration
import Feldspar.Compiler.Backend.C.Platforms
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Backend.C.Library
import Feldspar.Compiler.Error

import qualified Data.List as List (last, find, intersperse)
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
    transform t (line, col) (options, place, indent) x@(Variable name typ role inf) = Result (Variable name typ role newInf) (snd newInf) cRep
        where
            ((newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                code $ toC options place x
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return newInf

instance Transformable1 DebugToC [] Constant where
    transform1 t pos@(line, col) down@(options, place, indent) l = transform1' t pos down l ", " 0

instance Transformable DebugToC Constant where
    transform t pos@(line, col) down@(options, place, indent) const@(IntConst c _ inf1 inf2) = transformConst t pos down const (show c)

    transform t pos@(line, col) down@(options, place, indent) const@(FloatConst c inf1 inf2) = transformConst t pos down const (show c ++ "f")

    transform t pos@(line, col) down@(options, place, indent) const@(BoolConst False inf1 inf2) = transformConst t pos down const "0"

    transform t pos@(line, col) down@(options, place, indent) const@(BoolConst True inf1 inf2) = transformConst t pos down const "1"

    transform t (line, col) (options, place, indent) const@(ComplexConst real im inf1 inf2) 
        = case (List.find (\(t',_) -> t' == typeof const) $ values $ platform options) of
            Just (_,f) -> 
                Result (ComplexConst (result newReal) (result newIm) newInf newInf) (snd newInf) cRep 
                    where
                        ((newReal, newIm, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                            newReal <- complexTransform t (options, place, indent) real
                            newIm <- complexTransform t (options, place, indent) im
                            code $ f const
                            (_, nl, nc) <- StateMonad.get
                            let newInf = ((line, col), (nl, nc))
                            return (newReal, newIm, newInf)
            Nothing    -> 
                Result (ComplexConst (result newReal) (result newIm) newInf newInf) (snd newInf) cRep
                    where
                        ((newReal, newIm, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                            code "complex("
                            newReal <- monadicTransform' t (options, place, indent) real
                            code ","
                            newIm <- monadicTransform' t (options, place, indent) im
                            code ")"
                            (_, nl, nc) <- StateMonad.get
                            let newInf = ((line, col), (nl, nc))
                            return (newReal, newIm, newInf)

instance Transformable DebugToC ActualParameter where
    transform t pos@(line, col) down@(options, place, indent) act@(In param@(VarExpr (Variable _ (StructType _) _ _) _) inf) =
        transformActParam t pos down act AddressNeed_pl
    transform t pos@(line, col) down@(options, place, indent) act@(In param@(VarExpr (Variable _ (ArrayType _ _) _ _) _) inf) =
        transformActParam t pos down act AddressNeed_pl
    transform t pos@(line, col) down@(options, place, indent) act@(In _ _) = transformActParam t pos down act FunctionCallIn_pl
    transform t pos@(line, col) down@(options, place, indent) act@(Out _ _) = transformActParam t pos down act AddressNeed_pl
    transform t pos@(line, col) down@(options, place, indent) act@(TypeParameter _ _ _) = transformActParam t pos down act MainParameter_pl
    transform t pos@(line, col) down@(options, place, indent) act@(FunParameter _ _ _) = transformActParam t pos down act FunctionCallIn_pl

instance Transformable1 DebugToC [] Expression where
    transform1 t pos@(line, col) down@(options, place, indent) l = transform1' t pos down l ", " 0

instance Transformable DebugToC Expression where
    transform t (line, col) (options, place, indent) (VarExpr val inf) = Result (VarExpr (result newVal) newInf) (snd newInf) cRep
        where
            ((newVal, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                newVal <- monadicTransform' t (options, place, indent) val
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return (newVal, newInf)

    transform t (line, col) (options, place, indent) e@(ArrayElem name index inf1 inf2) = Result (ArrayElem (result newName) (result newIndex) newInf newInf) (snd newInf) cRep 
        where
            ((newName, newIndex, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                let prefix = case (place, typeof e) of
                       (AddressNeed_pl, _) -> "&"
                       (_, ArrayType _ _)  -> "&" -- TODO the call site should set the place to AddressNeed_pl for Arrays
                       _                   -> ""
                code $ prefix ++ "at(" ++ show_type options Value Declaration_pl (typeof e) NoRestrict ++ ","
                newName  <- monadicTransform' t (options, AddressNeed_pl, indent) name
                code ","
                newIndex <- monadicTransform' t (options, ValueNeed_pl, indent) index
                code ")"
                (_, newLine, newCol) <- StateMonad.get
                let newInf = ((line, col), (newLine, newCol))
                return (newName, newIndex, newInf)

    transform t pos@(line, col) down@(options, place, indent) expr@(StructField str field inf1 inf2) = transformExpr t pos down expr ("." ++ field) field ValueNeed_pl
      where
          transformExpr t (line, col) (options, place, indent) expr str field paramType = Result (newExpr expr) (snd newInf) cRep
            where
                newExpr (StructField e s _ _ ) = (StructField (result newTarget) s newInf newInf)
                getExpr (StructField e _ _ _ ) = e
                prefix = case (place, typeof expr) of
                       (AddressNeed_pl, _) -> "&"
                       (_, ArrayType _ _)  -> "&"
                       _                   -> ""
                ((newTarget, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                    code prefix
                    newTarget <- monadicTransform' t (options, paramType, indent)  (getExpr expr)
                    code str
                    (_, nl, nc) <- StateMonad.get
                    let newInf = ((line, col), (nl, nc))
                    return (newTarget, newInf)


    transform t (line, col) (options, place, indent) (ConstExpr val inf) = Result (ConstExpr (result newVal) newInf) (snd newInf) cRep
        where
            ((newVal, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                newVal <- monadicTransform' t (options, place, indent) val
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return (newVal, newInf)

    transform t pos@(line, col) down@(options, place, indent)
                fc@(FunctionCall f [a,b] inf1 inf2) | funName f == "!" =
                transformFuncCall t pos down fc "at(" "," ")"

    transform t pos@(line, col) down@(options, place, indent)
                fc@(FunctionCall f [a,b] inf1 inf2) | funMode f == Infix =
                transformFuncCall t pos down fc "(" (" " ++ funName f ++ " ") ")"

    transform t (line, col) (options, place, indent)
                (FunctionCall f paramlist inf1 inf2) =
                Result (FunctionCall f (result1 newParamlist) newInf newInf) (snd newInf) cRep 
        where
            ((newParamlist, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                code $ funName f ++ "("
                newParamlist <- monadicListTransform' t (options, FunctionCallIn_pl, indent) paramlist
                code ")"
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return (newParamlist, newInf)

    transform t (line, col) (options, place, indent) (Cast typ exp inf1 inf2) =  Result (Cast typ (result newExp) newInf newInf) (snd newInf) cRep 
        where
            ((newExp, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                code $ concat ["((", toC options place typ, ")("]
                newExp <- monadicTransform' t (options, place, indent) exp
                code "))"
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return (newExp, newInf)

    transform t (line, col) (options, place, indent) (SizeOf (Left typ) inf1 inf2) = Result (SizeOf (Left typ) newInf newInf) (snd newInf) cRep 
        where
            ((newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                code ("sizeof(" ++ toC options place typ ++ ")")
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return newInf

    transform t (line, col) (options, place, indent) (SizeOf (Right exp) inf1 inf2) = Result (SizeOf (Right (result newExp)) newInf newInf) (newLine, newCol) cRep 
        where
            ((newExp, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                code "sizeof"
                newExp <- monadicTransform' t (options, place, indent) exp
                code ")"
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return (newExp, newInf)

instance Transformable1 DebugToC [] Entity where
    transform1 t pos@(line, col) down@(options, place, indent) l = transform1' t pos down l "" 0

instance Transformable DebugToC Module where
    transform t (line, col) (options, place, indent) (Module defList inf) = Result (Module (result1 newDefList) newInf) (snd newInf) cRep 
        where
            ((newDefList, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                newDefList <- monadicListTransform' t (options, place, indent) defList
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return (newDefList, newInf)

instance Transformable1 DebugToC [] Variable where
    transform1 t pos@(line, col) down@(options, place, indent) l = transform1' t pos down l ", " 0

instance Transformable1 DebugToC [] StructMember where
    transform1 t (line, col) (options, place, indent) [] = Result1 [] (line, col) ""
    transform1 t (line, col) (options, place, indent) (x:xs) = Result1 ((result newX):(result1 newXs)) (state1 newXs) (cRep) where
        ((newX, newXs), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
            indenter indent
            newX <- monadicTransform' t (options, place, indent) x
            newXs <- monadicListTransform' t (options, place, indent) xs
            return (newX, newXs)


instance Transformable DebugToC Entity where
    transform t (line, col) (options, place, indent) (StructDef name members inf1 inf2) = Result (StructDef name (result1 newMembers) newInf newInf) (snd newInf) cRep
        where
            ((newMembers, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                code $ name ++ " {\n"
                (crep, cl, cc) <- StateMonad.get
                StateMonad.put (crep, cl, cc + (addIndent indent))
                newMembers <- monadicListTransform' t (options, place, addIndent indent) members
                indenter indent
                code "};\n"
                (crep, cl, _) <- StateMonad.get
                StateMonad.put (crep, cl, indent)
                let newInf = ((line, col), (cl, indent))
                return (newMembers, newInf)

    transform t (line, col) (options, place, indent) (TypeDef typ name inf) = Result (TypeDef typ name newInf) (snd newInf) cRep
        where
            (newInf, (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                code $ unwords [ "typedef"
                               , show_type options Value place typ NoRestrict
                               , name
                               ]
                code ";\n"
                (crep, cl, _) <- StateMonad.get
                StateMonad.put (crep, cl, indent)
                let newInf = ((line, col), (cl, indent))
                return newInf

    transform t (line, col) (options, place, indent) (ProcDef name inParam outParam body inf1 inf2) =
      Result (ProcDef name (result1 newInParam) (result1 newOutParam) (result newBody) newInf newInf) (snd newInf) cRep
        where
            ((newInParam, newOutParam, newBody, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                indenter indent
                code $ "void " ++ name ++ "("
                newInParam <- monadicListTransform' t (options, MainParameter_pl, indent) inParam
                let str
                        | null inParam || null outParam   = ""
                        | otherwise                       = ", "
                code str
                newOutParam <- monadicListTransform' t (options, MainParameter_pl, indent) outParam
                code $ ")\n"
                indenter indent
                -- code "{\nprintf(\"enter\\n\");\n"
                -- code "printf(\"out:%p buffer:%p len:%d esize:%d\\n\", out, out->buffer, out->length, out->elemSize);\n"
                code "{\n"
                (crep, al, _) <- StateMonad.get
                StateMonad.put (crep, al, addIndent indent)
                newBody <- monadicTransform' t (options, Declaration_pl, addIndent indent) body
                indenter indent
                code "}\n"
                -- code "printf(\"leave\\n\");\n}\n"
                (_, nl, _) <- StateMonad.get
                let newInf = ((line, col), (nl, indent))
                return (newInParam, newOutParam, newBody, newInf)

    transform t (line, col) (options, place, indent) (ProcDecl name inParam outParam inf1 inf2) =
      Result (ProcDecl name (result1 newInParam) (result1 newOutParam) newInf newInf) (snd newInf) cRep
        where
            ((newInParam, newOutParam, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                indenter indent
                code $ "void " ++ name ++ "("
                newInParam <- monadicListTransform' t (options, MainParameter_pl, indent) inParam
                let str
                        | null inParam || null outParam   = ""
                        | otherwise                       = ", "
                code str
                newOutParam <- monadicListTransform' t (options, MainParameter_pl, indent) outParam
                code $ ");\n"
                (_, nl, _) <- StateMonad.get
                let newInf = ((line, col), (nl, indent))
                return (newInParam, newOutParam, newInf)

displayComment :: Int -> [String] -> String
displayComment indent = unlines . map (putIndent indent ++) . (["/*"] ++) . (++ [" */"]) . map (" * " ++)

displayMemInfo :: String -> [Type] -> String
displayMemInfo name [] = name ++ ": none"
displayMemInfo name info = name ++ ": " ++ (concat $ List.intersperse ", " $ map displayType info)

displayType :: Type -> String
displayType (ArrayType (LiteralLen n) (ArrayType (LiteralLen m) t)) =
    unwords [displayType t, concat ["array(", show n, "x", show m, ")"]]
displayType (ArrayType (LiteralLen n) t) = unwords [displayType t, concat ["array(", show n, ")"]]
displayType (ArrayType UndefinedLen t) = unwords [displayType t, "array"]
displayType (IVarType t) = unwords ["ivar(", displayType t, ")"]
displayType VoidType = "void"
displayType BoolType = "Boolean"
displayType BitType = "bit"
displayType FloatType = "float"
displayType (NumType signed size) = unwords [sg signed, (sz size) ++ "-bit", "integer"]
  where
    sg Signed   = "signed"
    sg Unsigned = "unsigned"
    sz S8  = "8"
    sz S16 = "16"
    sz S32 = "32"
    sz S40 = "40"
    sz S64 = "64"
displayType (ComplexType t) = unwords ["complex", displayType t]
displayType (UserType s) = s
displayType (StructType fields) = "struct"

instance Transformable DebugToC StructMember where
    transform t (line, col) (options, place, indent) dsm@(StructMember str typ inf) = Result (StructMember str typ newInf) (snd newInf) cRep 
        where
            ((newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                let t = case structMemberType dsm of
                     ArrayType len innerType -> show_variable options place Value (structMemberType dsm)
                                                     (structMemberName dsm) ++ ";"
                     otherwise -> (toC options place $ structMemberType dsm) ++ " " ++ structMemberName dsm ++ ";"
                code $ t ++ "\n"
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return newInf

instance Transformable1 DebugToC [] Declaration where
    transform1 t (line, col) (options, place, indent) [] = Result1 [] (line, col) ""
    transform1 t (line, col) (options, place, indent) (x:xs) = Result1 ((result newX):(result1 newXs)) (state1 newXs) ((putIndent indent ++ up newX ++ ";\n" ) ++ up1 newXs) where
        newX = transform t (line, col) (options, place, indent) x
        (line2, col2) =  state newX
        newSt = (line2 + 1, indent) 
        newXs = transform1 t newSt (options, place, indent) xs

instance Transformable DebugToC Block where
    transform t (line, col) (options, place, indent) (Block locs body inf) = Result (Block (result1 newLocs) (result newBody) newInf) (snd newInf) cRep
        where
            ((newLocs, newBody, newInf), (cRep, newLine, newcol)) = flip StateMonad.runState (defaultState line col) $ do
                newLocs <- monadicListTransform' t (options, Declaration_pl, indent) locs
                let str = case up1 newLocs of 
                     ""             -> ""
                     otherwise     -> "\n"
                code str
                newBody <- monadicTransform' t (options, place, indent) body
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, indent))
                return (newLocs, newBody, newInf)

instance Transformable DebugToC Declaration where
    transform t (line, col) (options, place, indent) (Declaration declVar Nothing inf) = Result (Declaration (result newDeclVar) Nothing newInf) (snd newInf) cRep
        where
            ((newDeclVar, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                newDeclVar <- monadicTransform' t (options, Declaration_pl, indent) declVar
                case varType declVar of
                    (ArrayType _ _) -> code " = {0}"
                    _               -> code ""
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return (newDeclVar, newInf)

    transform t (line, col) (options, place, indent) (Declaration declVar (Just expr) inf) = Result (Declaration (result newDeclVar) (Just (result newExpr)) newInf) (snd newInf) cRep 
        where
            ((newDeclVar, newExpr, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                newDeclVar <- monadicTransform' t (options, Declaration_pl, indent) declVar
                code " = "
                newExpr <- monadicTransform' t (options, ValueNeed_pl, indent) expr
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return (newDeclVar, newExpr, newInf)

instance Transformable1 DebugToC [] ActualParameter where
    transform1 t pos@(line, col) down@(options, place, indent) l = transform1' t pos down l ", " 0

instance Transformable1 DebugToC [] Program where
    transform1 t pos@(line, col) down@(options, place, indent) l = transform1' t pos down l "" 0


instance Transformable DebugToC Program where
    transform t (line, col) (options, place, indent) (Empty inf1 inf2) = Result (Empty newInf newInf) newSt cRep where 
        newSt = (line, col)
        newInf = ((line, col), newSt)
        cRep = ""

    transform t (line, col) (options, place, indent) (Comment True comment inf1 inf2) = Result (Comment True comment newInf newInf) (snd newInf) cRep 
        where
            ((newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                indenter indent
                code $ "/* " ++ comment ++ " */\n"
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return (newInf)

    transform t (line, col) (options, place, indent) (Comment False comment inf1 inf2) = Result (Comment False comment newInf newInf) (snd newInf) cRep 
        where 
            ((newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                indenter indent
                code $ "// " ++ comment ++ "\n"
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return (newInf)

    transform t (line, col) (options, place, indent) (Assign lhs rhs inf1 inf2) = Result (Assign (result newLhs) (result newRhs) newInf newInf) (snd newInf) cRep
        where
            ((newLhs, newRhs, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                indenter indent
                newLhs <- monadicTransform' t (options, ValueNeed_pl, indent) lhs
                code " = "
                newRhs <- monadicTransform' t (options, ValueNeed_pl, indent) rhs
                code ";\n"
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, indent))
                return (newLhs, newRhs, newInf)

    transform t (line, col) (options, place, indent) (ProcedureCall name param inf1 inf2) = Result (ProcedureCall name (result1 newParam) newInf newInf) (snd newInf) cRep 
        where
            ((newParam, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                indenter indent
                code $ name ++ "("
                newParam <- monadicListTransform' t (options, place, indent) param
                code ");\n"
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, indent))
                return (newParam, newInf)

    transform t (line, col) (options, place, indent) (Sequence prog inf1 inf2) = Result (Sequence (result1 newProg) newInf newInf) (snd newInf) cRep 
        where
            ((newProg, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                newProg <- monadicListTransform' t (options, place, indent) prog
                let newInf = ((line, col), state1 newProg)
                return (newProg, newInf)

    transform t (line, col) (options, place, indent) (Branch con tPrg ePrg inf1 inf2) = Result (Branch (result newCon) (result newTPrg) (result newEPrg) newInf newInf) (snd newInf) cRep 
        where 
            ((newCon, newTPrg, newEPrg, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                indenter indent
                code "if("
                newCon <- monadicTransform' t (options, ValueNeed_pl, indent) con
                code ")\n" 
                indenter indent
                code "{\n"
                newTPrg <- monadicTransform' t (options, place, addIndent indent) tPrg
                indenter indent
                code "}\n" 
                indenter indent 
                code "else\n" 
                indenter indent 
                code "{\n"
                newEPrg <- monadicTransform' t (options, place, addIndent indent) ePrg
                indenter indent 
                code "}\n"
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return (newCon, newTPrg, newEPrg, newInf)

    transform t (line, col) (options, place, indent) (SeqLoop con conPrg blockPrg inf1 inf2) = Result (SeqLoop (result newCon) (result newConPrg) (result newBlockPrg) newInf newInf) (snd newInf) cRep 
        where
            ((newCon, newConPrg, newBlockPrg, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                indenter indent
                code "{\n"
                newConPrg <- monadicTransform' t (options, place, addIndent indent) conPrg
                indenter $ addIndent indent
                code "while("
                newCon <- monadicTransform' t (options, ValueNeed_pl, addIndent indent) con
                code $ ")\n" 
                indenter $ addIndent indent
                code "{\n"
                newBlockPrg <- monadicTransform' t (options, place, addIndent $ addIndent indent) blockPrg
                monadicTransform' t (options, place, addIndent $ addIndent indent) (blockBody conPrg)
                indenter $ addIndent indent
                code "}\n" 
                indenter indent 
                code "}\n"
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return (newCon, newConPrg, newBlockPrg, newInf)

    transform t (line, col) (options, place, indent) (ParLoop count bound step prog inf1 inf2) = Result (ParLoop (result newCount) (result newBound) step (result newProg) newInf newInf) (snd newInf) cRep 
        where
            ((newCount, newBound, newProg, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                indenter indent
                code "for("
                newCount <- monadicTransform' t (options, Declaration_pl, addIndent indent) count
                code $ " = 0; "
                loopVariable <- monadicTransform' t (options, ValueNeed_pl, addIndent indent) count
                code $ " < "
                newBound <- monadicTransform' t (options, ValueNeed_pl, addIndent indent) bound
                code $ "; " ++ up loopVariable ++ " += " ++ show step ++ ")\n" 
                indenter indent
                code "{\n"
                -- code "printf(\"before\\n\");\n"
                newProg <- monadicTransform' t (options, place, addIndent indent) prog
                -- code "printf(\"after\\n\");\n"
                indenter indent
                code "}\n" 
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (nl, nc))
                return (loopVariable, newBound, newProg, newInf)

    transform t (line, col) (options, place, indent) (BlockProgram prog inf) = Result (BlockProgram (result newProg) newInf) (snd newInf) cRep 
        where
            ((newProg, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
                indenter indent 
                code "{\n"
                newProg <- monadicTransform' t (options, place, addIndent indent) prog
                indenter indent 
                code "}\n"
                (_, nl, nc) <- StateMonad.get
                let newInf = ((line, col), (newLine, newCol))
                return (newProg, newInf)

putIndent ind = concat $ replicate ind " "

addIndent :: Int -> Int
addIndent indent = indent + 4

transform1' t (line, col) (options, place, indent) [] str num = Result1 [] (line, col) ""
transform1' t (line, col) (options, place, indent) (x:[]) str num = Result1 ((result newX):[]) (state newX) (up newX) where
    newX = transform t (line, col) (options, place, indent) x
transform1' t (line, col) (options, place, indent) (x:xs) str num = Result1 ((result newX):(result1 newXs)) (state1 newXs) (up newX ++ str ++ up1 newXs) where
    newX = transform t (line, col) (options, place, indent) x
    (line2, col2) =  state newX
    newSt = (line2 + num, col2 + length str) 
    newXs = transform1 t newSt (options, place, indent) xs

transformConst t (line, col) (options, place, indent) (const :: Constant ()) str = Result (newConst const) (line, newCol) cRep 
    where
        newConst (IntConst c t _ _) = (IntConst c t newInf newInf)
        newConst (FloatConst c _ _) = (FloatConst c newInf newInf)
        newConst (BoolConst c _ _) = (BoolConst c newInf newInf)
        newInf = ((line, col), (line, newCol))
        (cRep, newLine, newCol) = snd $ flip StateMonad.runState (defaultState line col) $ do
        let s = case (List.find (\(t',_) -> t' == typeof const) $ values $ platform options) of
             Just (_,f) -> f const
             Nothing    -> str
        code s

transformActParam t (line, col) (options, place, indent) (TypeParameter typ mode inf) _ = Result newParam (snd newInf) cRep 
    where
        newParam = TypeParameter typ mode newInf
        place Auto = MainParameter_pl
        place Scalar = Declaration_pl
        (newInf, (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
            code $ show_type options Value (place mode) typ NoRestrict
            (_, nl, nc) <- StateMonad.get
            return ((line, col), (nl, nc))

transformActParam t (line, col) (options, place, indent) (FunParameter name addr inf) _ = Result newParam (snd newInf) cRep 
    where
        newParam = FunParameter name addr newInf
        (newInf, (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
            let addrOp
                    | addr      = "&"
                    | otherwise = ""
            code $ addrOp ++ name
            (_, nl, nc) <- StateMonad.get
            return ((line, col), (nl, nc))

transformActParam t (line, col) (options, place, indent) act paramType = Result (newActParam act) (snd newInf) cRep 
    where
        newActParam (Out _ _) = (Out (result newParam) newInf)
        newActParam (In _ _) = (In (result newParam) newInf)
        getParam (In param _) = param
        getParam (Out param _) = param
        ((newParam, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
            newParam <- monadicTransform' t (options, paramType, indent) (getParam act)
            (_, nl, nc) <- StateMonad.get
            let newInf = ((line, col), (nl, nc))
            return (newParam, newInf)

transformFuncCall t (line, col) (options, place, indent)
                  (FunctionCall f [a, b] inf1 inf2) str1 str2 str3 =
                  Result (FunctionCall f [result newA, result newB] newInf newInf) (snd newInf) cRep
    where
        ((newA, newB, newInf), (cRep, newLine, newCol)) = flip StateMonad.runState (defaultState line col) $ do
            code str1
            newA <- monadicTransform' t (options, place, indent) a
            code str2
            newB <- monadicTransform' t (options, place, indent) b
            code str3
            (_, nl, nc) <- StateMonad.get
            let newInf = ((line, col), (nl, nc))
            return (newA, newB, newInf)

code s = do
    (str, line, col) <- StateMonad.get
    let numEOF = length (filter (=='\n') s)
    StateMonad.put (str++s,line + numEOF, (if numEOF == 0 then col else 0) + (length $ takeWhile (/='\n') $ reverse s))

indenter n = do
    (str, line, col) <- StateMonad.get
    StateMonad.put (str ++ (concat $ replicate n " "), line, col)

monadicTransform' t down@(options, place, indent) d = do
    (cRep, line, col) <- StateMonad.get
    let res = transform t (line, col) down d
    code $ up res
    return res

complexTransform t down@(options, place, indent) d = do
    (cRep, line, col) <- StateMonad.get
    let res = transform t (line, col) down d
    return res

monadicListTransform' t down@(options, place, indent) l = do
    (_, line, col) <- StateMonad.get
    let resList = transform1 t (line, col) down l
    code $ up1 resList
    return resList

defaultState :: Int -> Int -> (String, Int, Int)
defaultState line col = ("", line, col)
