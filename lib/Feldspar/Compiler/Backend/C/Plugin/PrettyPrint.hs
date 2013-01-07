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
{-# LANGUAGE RecordWildCards #-}
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

data PrintEnv = PEnv
    { -- Platform, Place and Indentation
      options :: Options,
      place :: Place,
      indent :: Int
     }
     deriving Show

instance Transformation DebugToC where
    type From DebugToC    = ()
    type To DebugToC      = DebugToCSemanticInfo
    type Down DebugToC    = PrintEnv
    type Up DebugToC      = String
    type State DebugToC   = (Int, Int)

instance Plugin DebugToC where
    type ExternalInfo DebugToC = ((Options, Place), Int)
    executePlugin DebugToC ((opts, plc), line) procedure =
        result $ transform DebugToC (line, 0)
                           (PEnv {options = opts, place = plc, indent = line})
                           procedure

compToCWithInfos :: Options -> Int -> Module () -> CompToCCoreResult DebugToCSemanticInfo
compToCWithInfos opts line procedure =
  CompToCCoreResult {
    sourceCode      = up res
  , endPosition     = state res
  , debugModule     = result res
  } where
    res = transform DebugToC (line, 0)
                    (PEnv {options = opts, place = Declaration_pl, indent = 0}) procedure

instance Transformable DebugToC Variable where
    transform _ pos (PEnv {..}) x@(Variable vname typ role) = Result (Variable vname typ role) (snd newInf) cRep
        where
            (newInf, (cRep, _)) = runState pos $ do
                code $ toC options place x
                (_, np) <- StateMonad.get
                return (pos, np)

instance Transformable1 DebugToC [] Constant where
    transform1 t pos down l = transform1' t pos down l ", "

instance Transformable DebugToC Constant where
    transform t pos down cnst@(IntConst c _) = transformConst pos down cnst (show c)

    transform t pos down cnst@(FloatConst c) = transformConst pos down cnst (show c ++ "f")

    transform t pos down cnst@(BoolConst False) = transformConst pos down cnst "0"

    transform t pos down cnst@(BoolConst True) = transformConst pos down cnst "1"

    transform t pos down@(PEnv {..}) cnst@(ComplexConst real im)
        = case List.find (\(t',_) -> t' == typeof cnst) $ values $ platform options of
            Just (_,f) -> 
                Result (ComplexConst (result newReal) (result newIm)) (snd newInf) cRep 
                    where
                        ((newReal, newIm, newInf), (cRep, _)) = runState pos $ do
                            nr <- complexTransform t down real
                            ni <- complexTransform t down im
                            code $ f cnst
                            (_, np) <- StateMonad.get
                            return (nr, ni, (pos,np))
            Nothing    -> 
                Result (ComplexConst (result newReal) (result newIm)) (snd newInf) cRep
                    where
                        ((newReal, newIm, newInf), (cRep, _)) = runState pos $ do
                            code "complex("
                            nr <- monadicTransform' t down real
                            code ","
                            ni <- monadicTransform' t down im
                            code ")"
                            (_, np) <- StateMonad.get
                            return (nr, ni, (pos,np))

instance Transformable DebugToC ActualParameter where
    transform t pos down act@(In (VarExpr var))
      | StructType{} <- typeof var = transformActParam t pos down act AddressNeed_pl
      | ArrayType{}  <- typeof var = transformActParam t pos down act AddressNeed_pl
    transform t pos down act@In{}            = transformActParam t pos down act FunctionCallIn_pl
    transform t pos down act@Out{}           = transformActParam t pos down act AddressNeed_pl
    transform t pos down act@TypeParameter{} = transformActParam t pos down act MainParameter_pl
    transform t pos down act@FunParameter{}  = transformActParam t pos down act FunctionCallIn_pl

instance Transformable1 DebugToC [] Expression where
    transform1 t pos down l = transform1' t pos down l ", "

instance Transformable DebugToC Expression where
    transform t pos down (VarExpr val) = Result (VarExpr (result newVal)) (snd newInf) cRep
        where
            ((newVal, newInf), (cRep, _)) = runState pos $ do
                nv <- monadicTransform' t down val
                (_, np) <- StateMonad.get
                return (nv, (pos,np))

    transform t pos down@(PEnv {..}) e@(ArrayElem n index) = Result (ArrayElem (result newName) (result newIndex)) (snd newInf) cRep 
        where
            ((newName, newIndex, newInf), (cRep, _)) = runState pos $ do
                let prefix = case (place, typeof e) of
                       (AddressNeed_pl, _) -> "&"
                       (_, ArrayType _ _)  -> "&" -- TODO the call site should set the place to AddressNeed_pl for Arrays
                       _                   -> ""
                code $ prefix ++ "at(" ++ showType options Value Declaration_pl (typeof e) NoRestrict ++ ","
                nn <- monadicTransform' t (newPlace down AddressNeed_pl) n
                code ","
                ni <- monadicTransform' t (newPlace down ValueNeed_pl) index
                code ")"
                (_, np) <- StateMonad.get
                return (nn, ni, (pos,np))

    transform t pos down@(PEnv {..}) e@(NativeElem n index) = Result (NativeElem (result newName) (result newIndex)) (snd newInf) cRep
        where
            ((newName, newIndex, newInf), (cRep, _)) = runState pos $ do
                let prefix = case (place, typeof e) of
                       (AddressNeed_pl, _) -> "&"
                       (_, ArrayType _ _)  -> "&" -- TODO the call site should set the place to AddressNeed_pl for Arrays
                       _                   -> ""
                -- No address needed since that is handled by the brackets.
                nn <- monadicTransform' t down n
                code "["
                ni <- monadicTransform' t (newPlace down ValueNeed_pl) index
                code "]"
                (_, np) <- StateMonad.get
                return (nn, ni, (pos,np))

    transform t pos down expr@(StructField _ field) = transformExpr pos down ('.' : field) ValueNeed_pl
      where
          transformExpr pos down@(PEnv {..}) str paramType = Result (newExpr expr) (snd newInf) cRep
            where
                newExpr (StructField _ s) = StructField (result newTarget) s
                getExpr (StructField e _) = e
                prefix = case (place, typeof expr) of
                       (AddressNeed_pl, _) -> "&"
                       (_, ArrayType _ _)  -> "&"
                       _                   -> ""
                ((newTarget, newInf), (cRep, _)) = runState pos $ do
                    code prefix
                    nt <- monadicTransform' t (newPlace down paramType)  (getExpr expr)
                    code str
                    (_, np) <- StateMonad.get
                    return (nt, (pos,np))


    transform t pos down (ConstExpr val) = Result (ConstExpr (result newVal)) (snd newInf) cRep
        where
            ((newVal, newInf), (cRep, _)) = runState pos $ do
                nv <- monadicTransform' t down val
                (_, np) <- StateMonad.get
                return (nv, (pos,np))

    transform t pos down fc@(FunctionCall f [_,_])
        | funName f == "!" = transformFuncCall t pos down fc "at(" "," ")"

    transform t pos down fc@(FunctionCall f [_,_])
        | funMode f == Infix = transformFuncCall t pos down fc "(" (" " ++ funName f ++ " ") ")"

    transform t pos down (FunctionCall f paramlist) =
                Result (FunctionCall f (result1 newParamlist)) (snd newInf) cRep 
        where
            ((newParamlist, newInf), (cRep, _)) = runState pos $ do
                code $ funName f ++ "("
                npl <- monadicListTransform' t (newPlace down FunctionCallIn_pl) paramlist
                code ")"
                (_, np) <- StateMonad.get
                return (npl, (pos,np))

    transform t pos down@(PEnv {..}) (Cast typ e) =  Result (Cast typ (result newExp)) (snd newInf) cRep 
        where
            ((newExp, newInf), (cRep, _)) = runState pos $ do
                code $ concat ["((", toC options place typ, ")("]
                ne <- monadicTransform' t down e
                code "))"
                (_, np) <- StateMonad.get
                return (ne, (pos,np))

    transform _ pos (PEnv {..}) (SizeOf (Left typ)) = Result (SizeOf (Left typ)) (snd newInf) cRep 
        where
            (newInf, (cRep, _)) = runState pos $ do
                code ("sizeof(" ++ toC options place typ ++ ")")
                (_, np) <- StateMonad.get
                return (pos, np)

    transform t pos down (SizeOf (Right e)) = Result (SizeOf (Right (result newExp))) p cRep 
        where
            ((newExp, newInf), (cRep, p)) = runState pos $ do
                code "sizeof"
                ne <- monadicTransform' t down e
                code ")"
                (_, np) <- StateMonad.get
                return (ne, (pos,np))

instance Transformable1 DebugToC [] Entity where
    transform1 t pos down l = transform1' t pos down l ""

instance Transformable DebugToC Module where
    transform t pos down (Module defList) = Result (Module (result1 newDefList)) (snd newInf) cRep 
        where
            ((newDefList, newInf), (cRep, _)) = runState pos $ do
                ndl <- monadicListTransform' t down defList
                (_, np) <- StateMonad.get
                return (ndl, (pos,np))

instance Transformable1 DebugToC [] Variable where
    transform1 t pos down l = transform1' t pos down l ", "

instance Transformable1 DebugToC [] StructMember where
    transform1 _ pos _ [] = Result1 [] pos ""
    transform1 t pos down (x:xs) = Result1 (result newX : result1 newXs) (state1 newXs) cRep where
        ((newX, newXs), (cRep, _)) = runState pos $ do
            indenter down
            nx  <- monadicTransform' t down x
            nxs <- monadicListTransform' t down xs
            return (nx, nxs)


instance Transformable DebugToC Entity where
    transform t pos down@(PEnv {..}) (StructDef n members) = Result (StructDef n (result1 newMembers)) (snd newInf) cRep
        where
            ((newMembers, newInf), (cRep, _)) = runState pos $ do
                code $ n ++ " {\n"
                (crep, (cl, cc)) <- StateMonad.get
                StateMonad.put (crep, (cl, cc + (trd $ addIndent down)))
                nms <- monadicListTransform' t (addIndent down) members
                indenter down
                code "};\n"
                (crep, (cl, _)) <- StateMonad.get
                StateMonad.put (crep, (cl, indent))
                return (nms, (pos,(cl,indent)))

    transform _ pos (PEnv {..}) (TypeDef typ n) = Result (TypeDef typ n) (snd newInf) cRep
        where
            (newInf, (cRep, _)) = runState pos $ do
                code $ unwords [ "typedef"
                               , showType options Value place typ NoRestrict
                               , n
                               ]
                code ";\n"
                (crep, (cl, _)) <- StateMonad.get
                StateMonad.put (crep, (cl, indent))
                return (pos, (cl, indent))

    transform t pos down@(PEnv {..}) (ProcDef n k inp outp body) =
      Result (ProcDef n k (result1 newInParam) (result1 newOutParam) (result newBody)) (snd newInf) cRep
        where
            ((newInParam, newOutParam, newBody, newInf), (cRep, _)) = runState pos $ do
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
                (crep, (al, _)) <- StateMonad.get
                StateMonad.put (crep, (al, trd $ addIndent down))
                nb <- monadicTransform' t (newPlace (addIndent down) Declaration_pl) body
                indenter down
                code "}\n"
                (_, (nl, _)) <- StateMonad.get
                return (ninp, noutp, nb, (pos,(nl,indent)))

    transform t pos down@(PEnv {..}) (ProcDecl n knd inp outp) =
      Result (ProcDecl n knd (result1 newInParam) (result1 newOutParam)) (snd newInf) cRep
        where
            ((newInParam, newOutParam, newInf), (cRep, _)) = runState pos $ do
                indenter down
                code $ "void " ++ n ++ "("
                ninp <- monadicListTransform' t (newPlace down MainParameter_pl) inp
                let str
                        | null inp || null outp = ""
                        | otherwise             = ", "
                code str
                noutp <- monadicListTransform' t (newPlace down MainParameter_pl) outp
                code ");\n"
                (_, (nl, _)) <- StateMonad.get
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
    transform _ pos (PEnv {..}) dsm@(StructMember str typ) = Result (StructMember str typ) (snd newInf) cRep 
        where
            (newInf, (cRep, _)) = runState pos $ do
                let t = case structMemberType dsm of
                     ArrayType{} -> showVariable options place Value (structMemberType dsm) (structMemberName dsm) ++ ";"
                     _           -> toC options place (structMemberType dsm) ++ " " ++ structMemberName dsm ++ ";"
                code $ t ++ "\n"
                (_, np) <- StateMonad.get
                return (pos, np)

instance Transformable1 DebugToC [] Declaration where
    transform1 _ pos _ [] = Result1 [] pos ""
    transform1 t pos down@(PEnv {..}) (x:xs) = Result1 (result newX : result1 newXs) (state1 newXs) ((putIndent indent ++ up newX ++ ";\n" ) ++ up1 newXs) where
        newX = transform t pos down x
        (line2, _) =  state newX
        newSt = (line2 + 1, indent) 
        newXs = transform1 t newSt down xs

instance Transformable DebugToC Block where
    transform t pos down@(PEnv {..}) (Block locs body) = Result (Block (result1 newLocs) (result newBody)) (snd newInf) cRep
        where
            ((newLocs, newBody, newInf), (cRep, _)) = runState pos $ do
                nlocs <- monadicListTransform' t (newPlace down Declaration_pl) locs
                let str = case up1 newLocs of 
                     "" -> ""
                     _  -> "\n"
                code str
                nbody <- monadicTransform' t down body
                (_, (nl, _)) <- StateMonad.get
                return (nlocs, nbody, (pos,(nl,indent)))

instance Transformable DebugToC Declaration where
    transform t pos down (Declaration dv Nothing) = Result (Declaration (result newDeclVar) Nothing) (snd newInf) cRep
        where
            ((newDeclVar, newInf), (cRep, _)) = runState pos $ do
                ndv <- monadicTransform' t (newPlace down Declaration_pl) dv
                case varType dv of
                    (ArrayType _ _) -> code " = {0}"
                    _               -> code ""
                (_, np) <- StateMonad.get
                return (ndv, (pos,np))

    transform t pos down (Declaration dv (Just e)) = Result (Declaration (result newDeclVar) (Just (result newExpr))) (snd newInf) cRep 
        where
            ((newDeclVar, newExpr, newInf), (cRep, _)) = runState pos $ do
                ndv <- monadicTransform' t (newPlace down Declaration_pl) dv
                code " = "
                ne <- monadicTransform' t (newPlace down ValueNeed_pl) e
                (_, np) <- StateMonad.get
                return (ndv, ne, (pos,np))

instance Transformable1 DebugToC [] ActualParameter where
    transform1 t pos down l = transform1' t pos down l ", "

instance Transformable1 DebugToC [] Program where
    transform1 t pos down l = transform1' t pos down l ""


instance Transformable DebugToC Program where
    transform _ pos _ Empty = Result Empty pos "" where 
        newInf = (pos, pos)

    transform _ pos down (Comment True comment) = Result (Comment True comment) (snd newInf) cRep 
        where
            (newInf, (cRep, _)) = runState pos $ do
                indenter down
                code $ "/* " ++ comment ++ " */\n"
                (_, np) <- StateMonad.get
                return (pos, np)

    transform _ pos down (Comment False comment) = Result (Comment False comment) (snd newInf) cRep 
        where 
            (newInf, (cRep, _)) = runState pos $ do
                indenter down
                code $ "// " ++ comment ++ "\n"
                (_, np) <- StateMonad.get
                return (pos, np)

    transform t pos down@(PEnv {..}) (Assign lh rh) = Result (Assign (result newLhs) (result newRhs)) (snd newInf) cRep
        where
            ((newLhs, newRhs, newInf), (cRep, _)) = runState pos $ do
                indenter down
                nlhs <- monadicTransform' t (newPlace down ValueNeed_pl) lh
                code " = "
                nrhs <- monadicTransform' t (newPlace down ValueNeed_pl) rh
                code ";\n"
                (_, (nl, _)) <- StateMonad.get
                return (nlhs, nrhs, (pos,(nl,indent)))

    transform t pos down@(PEnv {..}) (ProcedureCall n k param) = Result (ProcedureCall n k (result1 newParam)) (snd newInf) cRep 
        where
            ((newParam, newInf), (cRep, _)) = runState pos $ do
                indenter down
                code $ n ++ "("
                np <- monadicListTransform' t down param
                code ");\n"
                (_, (nl, _)) <- StateMonad.get
                return (np, (pos,(nl,indent)))

    transform t pos down (Sequence prog) = Result (Sequence (result1 newProg)) (snd newInf) cRep 
        where
            ((newProg, newInf), (cRep, _)) = runState pos $ do
                np <- monadicListTransform' t down prog
                return (np, (pos,state1 newProg))

    transform t pos down (Branch con tPrg ePrg) = Result (Branch (result newCon) (result newTPrg) (result newEPrg)) (snd newInf) cRep 
        where 
            ((newCon, newTPrg, newEPrg, newInf), (cRep, _)) = runState pos $ do
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
                (_, np) <- StateMonad.get
                return (ncon, ntPrg, nePrg, (pos,np))

    transform t pos down (Switch scrut alts) = error "TODO: PrettyPrint for switch"

    transform t pos down (SeqLoop con conPrg blockPrg) = Result (SeqLoop (result newCon) (result newConPrg) (result newBlockPrg)) (snd newInf) cRep 
        where
            ((newCon, newConPrg, newBlockPrg, newInf), (cRep, _)) = runState pos $ do
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
                (_, np) <- StateMonad.get
                return (ncon, ncp, nbp, (pos,np))

    transform t pos down (ParLoop count bound step prog) = Result (ParLoop (result newCount) (result newBound) step (result newProg)) (snd newInf) cRep 
        where
            ((newCount, newBound, newProg, newInf), (cRep, _)) = runState pos $ do
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
                (_, np') <- StateMonad.get
                return (loopVariable, nb, np, (pos,np'))

    transform t pos down (BlockProgram prog) = Result (BlockProgram (result newProg)) (snd newInf) cRep 
        where
            ((newProg, newInf), (cRep, _)) = runState pos $ do
                indenter down
                code "{\n"
                np <- monadicTransform' t (addIndent down) prog
                indenter down
                code "}\n"
                (_, np') <- StateMonad.get
                return (np, (pos,np'))

putIndent :: Int -> String
putIndent = concat . flip replicate " "

addIndent :: PrintEnv -> PrintEnv
addIndent p@(PEnv {..}) = p { indent = indent + 4}

transform1' _ pos _ [] _ = Result1 [] pos ""
transform1' t pos down (x:[]) _ = Result1 [result newX] (state newX) (up newX) where
    newX = transform t pos down x
transform1' t pos down (x:xs) str = Result1 (result newX : result1 newXs) (state1 newXs) (up newX ++ str ++ up1 newXs) where
    newX = transform t pos down x
    (line2, col2) =  state newX
    newSt = (line2, col2 + length str)
    newXs = transform1 t newSt down xs

transformConst pos@(line, _) (PEnv {..}) (cnst :: Constant ()) str = Result (newConst cnst) (line, newCol) cRep 
    where
        newConst (IntConst c t) = IntConst c t
        newConst (FloatConst c) = FloatConst c
        newConst (BoolConst c)  = BoolConst c
        newInf = (pos, (line, newCol))
        (cRep, (_, newCol)) = snd $ runState pos $ do
        let s = case List.find (\(t',_) -> t' == typeof cnst) $ values $ platform options of
             Just (_,f) -> f cnst
             Nothing    -> str
        code s

transformActParam _ pos (PEnv {..}) (TypeParameter typ mode) _ = Result newParam (snd newInf) cRep 
    where
        newParam = TypeParameter typ mode
        place Auto = MainParameter_pl
        place Scalar = Declaration_pl
        (newInf, (cRep, _)) = runState pos $ do
            code $ showType options Value (place mode) typ NoRestrict
            (_, np) <- StateMonad.get
            return (pos,np)

transformActParam _ pos _ (FunParameter n k addr) _ = Result newParam (snd newInf) cRep 
    where
        newParam = FunParameter n k addr
        (newInf, (cRep, _)) = runState pos $ do
            let addrOp
                    | addr      = "&"
                    | otherwise = ""
            code $ addrOp ++ n
            (_, np) <- StateMonad.get
            return (pos, np)

transformActParam t pos down act paramType = Result (newActParam act) (snd newInf) cRep 
    where
        newActParam Out{} = Out (result newParam)
        newActParam In{}  = In  (result newParam)
        getParam (In param)  = param
        getParam (Out param) = param
        ((newParam, newInf), (cRep, _)) = runState pos $ do
            np <- monadicTransform' t (newPlace down paramType) (getParam act)
            (_, np') <- StateMonad.get
            return (np, (pos,np'))

transformFuncCall t pos down
                  (FunctionCall f [a, b]) str1 str2 str3 =
                  Result (FunctionCall f [result newA, result newB]) (snd newInf) cRep
    where
        ((newA, newB, newInf), (cRep, _)) = runState pos $ do
            code str1
            na <- monadicTransform' t down a
            code str2
            nb <- monadicTransform' t down b
            code str3
            (_, np) <- StateMonad.get
            return (na, nb, (pos,np))

code s = do
    (str, (line, col)) <- StateMonad.get
    let numEOF = length (filter (=='\n') s)
    StateMonad.put (str++s,(line + numEOF, (if numEOF == 0 then col else 0) + length (takeWhile (/='\n') $ reverse s)))

indenter (PEnv {..}) = do
    (str, p) <- StateMonad.get
    StateMonad.put (str ++ concat (replicate indent " "), p)

monadicTransform' t down d = do
    (_, p) <- StateMonad.get
    let res = transform t p down d
    code $ up res
    return res

complexTransform t down d = do
    (_, p) <- StateMonad.get
    return $ transform t p down d

monadicListTransform' t down l = do
    (_, p) <- StateMonad.get
    let resList = transform1 t p down l
    code $ up1 resList
    return resList

defaultState :: (Int, Int) -> (String, (Int, Int))
defaultState pos = ("", pos)

trd :: PrintEnv -> Int
trd (PEnv {..}) = indent

newPlace :: PrintEnv -> Place -> PrintEnv
newPlace env plc = env {place = plc}

runState pos b = flip StateMonad.runState (defaultState pos) b
