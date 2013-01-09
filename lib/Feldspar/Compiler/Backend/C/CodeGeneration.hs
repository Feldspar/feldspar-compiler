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

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Feldspar.Compiler.Backend.C.CodeGeneration where

import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Error (handleError, ErrorClass(..))
import Feldspar.Compiler.Backend.C.Options

import Text.PrettyPrint

import Debug.Trace

codeGenerationError :: ErrorClass -> String -> a
codeGenerationError = handleError "CodeGeneration"

data PrintEnv = PEnv
    { options :: Options
    , place :: Place
    }
  deriving Show

compToCWithInfos :: Options -> Int -> Module () -> CompToCCoreResult ()
compToCWithInfos opts _ procedure =
  CompToCCoreResult { sourceCode  = render $ ppr (PEnv opts Declaration_pl) procedure
                    , debugModule = procedure
                    }

class Pretty a
  where
    ppr     :: PrintEnv -> a -> Doc
    pprList :: PrintEnv -> [a] -> Doc

instance Pretty (Module ())
  where
    ppr env Module{..} = pprList env entities
    pprList env = sep . map (ppr env)

instance Pretty (Entity ())
  where
    ppr env StructDef{..} = text "struct"  <+> text structName    $+$ block (pprList env structMembers) <> semi
    ppr env TypeDef{..}   = text "typedef" <+> ppr env actualType <+> text typeName <> semi
    ppr env ProcDef{..}   = text "void"    <+> text procName      <>  parens (pprList (newPlace env MainParameter_pl) $ inParams ++ outParams) $$ block (ppr env procBody)
    ppr env ProcDecl{..}  = text "void"    <+> text procName      <>  parens (pprList (newPlace env MainParameter_pl) $ inParams ++ outParams) <> semi

    pprList env = vcat . punctuate (text "\n") . map (ppr env)

instance Pretty (StructMember ())
  where
    ppr env StructMember{..} = ppr env structMemberType <+> text structMemberName <> sizeInBrackets structMemberType <> semi

    pprList env = vcat . map (ppr env)

instance Pretty (Block ())
  where
    ppr env Block{..} = sep [ pprList (newPlace env Declaration_pl) locals
                            , if (null locals) then empty else text ""
                            , ppr env blockBody
                            ]
    pprList env = sep . punctuate (text "\n") . map (ppr env)

instance Pretty (Declaration ())
  where
    ppr env Declaration{..} = ppr (newPlace env Declaration_pl) declVar <+> nest 2 init <> semi
      where
        init = case (initVal, varType declVar) of
                 (Just i, _)           -> equals <+> ppr (newPlace env ValueNeed_pl) i
                 (_     , ArrayType{}) -> equals <+> braces (int 0)
                 _                     -> empty
    pprList env = vcat . map (ppr env)

instance Pretty (Program ())
  where
    ppr env Empty = empty
    ppr env Comment{..}
      | isBlockComment = blockComment $ map text $ lines commentValue
      | otherwise      = text "//" <+> text commentValue
    ppr env Assign{..} = ppr env' lhs <+> equals <+> nest 2 (ppr env' rhs) <> semi
      where
        env' = newPlace env ValueNeed_pl
    ppr env ProcedureCall{..} = stmt $ call (text procCallName) (map (ppr env) procCallParams)
    ppr env Sequence{..} = pprList env sequenceProgs
    ppr env Branch{..} =  text "if" <+> parens (ppr (newPlace env ValueNeed_pl) branchCond)
                       $$ block (ppr env thenBlock)
                       $$ text "else"
                       $$ block (ppr env elseBlock)
    ppr env Switch{..} =  text "switch" <+> parens (ppr env scrutinee)
                       $$ block (vcat [ ppr env p $$ nest 2 (ppr env b) | (p, b) <- alts])
    ppr env SeqLoop{..} =  ppr env sLoopCondCalc
                        $$ text "while" <+> parens (ppr (newPlace env ValueNeed_pl) sLoopCond)
                        $$ block (ppr env sLoopBlock $+$ ppr env sLoopCondCalc)
    ppr env ParLoop{..} =  text "for" <+> parens (sep $ map (nest 4) $ punctuate semi [init, guard, next])
                        $$ block (ppr env pLoopBlock)
      where
        ixd   = ppr envd pLoopCounter
        ixv   = ppr envv pLoopCounter
        init  = ixd <+> equals    <+> int 0
        guard = ixv <+> char '<'  <+> ppr envv pLoopBound
        next  = ixv <+> text "+=" <+> int pLoopStep
        envd  = newPlace env Declaration_pl
        envv  = newPlace env ValueNeed_pl

    ppr env BlockProgram{..} = block (ppr env blockProgram)

    pprList env = vcat . map (ppr env)

instance Pretty (Pattern ())
  where
    ppr env PatDefault = text "default" <> colon
    ppr env (Pat c)    = ppr env c <> colon

    pprList env = vcat . map (ppr env)

instance Pretty (ActualParameter ())
  where
    ppr env (In VarExpr{..})
      | ArrayType{}  <- typeof var = ppr (newPlace env AddressNeed_pl) var
      | StructType{} <- typeof var = ppr (newPlace env AddressNeed_pl) var
    ppr env In{..}                 = ppr (newPlace env FunctionCallIn_pl) inParam
    ppr env Out{..}                = ppr (newPlace env AddressNeed_pl) outParam
    ppr env TypeParameter{..}      = ppr env typeParam
    ppr env FunParameter{..}       = prefix <> text funParamName
      where prefix = if addressNeeded then char '&' else empty
    pprList env = hsep . punctuate comma . map (ppr env)

instance Pretty (Expression ())
  where
    ppr env VarExpr{..} = ppr env var
    ppr env e@ArrayElem{..}  =  prefix <> (text "at")
                             <> parens (hcat $ punctuate comma [ ppr env $ typeof e
                                                               , ppr (newPlace env AddressNeed_pl) array
                                                               , ppr (newPlace env ValueNeed_pl) arrayIndex
                                                               ])
      where
        prefix = case (place env, typeof e) of
                   (AddressNeed_pl, _) -> char '&'
                   (_, ArrayType _ _)  -> char '&' -- TODO the call site should set the place to AddressNeed_pl for Arrays
                   _                   -> empty
    ppr env e@NativeElem{..} = prefix <> ppr (newPlace env AddressNeed_pl) array <> brackets (ppr (newPlace env ValueNeed_pl) arrayIndex)
      where
        prefix = case (place env, typeof e) of
                   (AddressNeed_pl, _) -> char '&'
                   (_, ArrayType _ _)  -> char '&' -- TODO the call site should set the place to AddressNeed_pl for Arrays
                   _                   -> empty
    ppr env e@StructField{..} = prefix <> ppr (newPlace env ValueNeed_pl) struct <> char '.' <> text fieldName
      where
        prefix = case (place env, typeof e) of
                   (AddressNeed_pl, _) -> char '&'
                   (_, ArrayType _ _)  -> char '&' -- TODO the call site should set the place to AddressNeed_pl for Arrays
                   _                   -> empty
    ppr env ConstExpr{..} = ppr env constExpr
    ppr env FunctionCall{..} | funName function == "!"   = call (text "at") $ map (ppr (newPlace env FunctionCallIn_pl)) funCallParams
                             | funMode function == Infix
                             , [a,b] <- funCallParams    = parens (ppr env a <+> text (funName function) <+> ppr env b)
                             | otherwise                 = call (text $ funName function) $ map (ppr (newPlace env FunctionCallIn_pl)) funCallParams
    ppr env Cast{..} = parens $ parens (ppr env castType) <> parens (ppr env castExpr)
    ppr env SizeOf{..} = call (text "sizeof") [either (ppr env) (ppr env) sizeOf]

    pprList env = sep . punctuate comma . map (ppr env)

instance Pretty (Variable t)
  where
    ppr env Variable{..} = case place env of
        MainParameter_pl -> typ <+> (ref <> name <> size)
        Declaration_pl   -> typ <+> (ref <> name <> size)
        _                -> ref <> name
      where
        typ  = ppr env varType
        size = sizeInBrackets varType
        ref  = case (varRole, place env, passByReference varType) of
                 (Ptr, AddressNeed_pl,    _    ) -> empty
                 (Ptr, MainParameter_pl,  _    ) -> empty
                 (Ptr, _,                 _    ) -> char '*'
                 (_,   MainParameter_pl,  True ) -> char '*'
                 (Val, AddressNeed_pl,    _    ) -> char '&'
                 (Val, FunctionCallIn_pl, True ) -> char '&'
                 _                               -> empty
        name = text varName
        passByReference ArrayType{}   = True
        passByReference StructType{}  = True
        passByReference _             = False

    pprList env = hsep . punctuate comma . map (ppr env)

instance Pretty (Constant ())
  where
    ppr env cnst@(IntConst c _)    = maybe (integer c) text $ transformConst env cnst
    ppr env cnst@(FloatConst c)    = maybe (double c)  text $ transformConst env cnst
    ppr env cnst@(BoolConst False) = maybe (int 0)     text $ transformConst env cnst
    ppr env cnst@(BoolConst True)  = maybe (int 1)     text $ transformConst env cnst
    ppr env cnst@ComplexConst{..}  = maybe cmplxCnst   text $ transformConst env cnst
      where
        cmplxCnst = text "complex" <> parens (pprList env [realPartComplexValue, imagPartComplexValue])
    pprList env = sep . punctuate comma . map (ppr env)

transformConst :: PrintEnv -> Constant () -> Maybe String 
transformConst PEnv{..} cnst = do
    f <- lookup (typeof cnst) $ values $ platform options
    return $ f cnst

instance Pretty Type
  where
    ppr env = toC
      where
        toC VoidType                   = text "void"
        toC ArrayType{}                = text "struct array"
        toC IVarType{}                 = text "struct ivar"
        toC (UserType u)               = text u
        toC (StructType n _)           = text "struct" <+> text n
        toC (NativeArray _ t)          = toC t
        toC t | Just s <- lookup t pts = text s
        toC t = codeGenerationError InternalError
              $ unwords ["Unhandled type in platform ", name pfm,  ": ", show t]
        pfm = platform $ options env
        pts = types pfm

    pprList env = sep . map (ppr env)

call :: Doc -> [Doc] -> Doc
call fn args = fn <> parens (hsep $ punctuate comma args)

blockComment :: [Doc] -> Doc
blockComment ds = vcat (zipWith (<+>) (text "/*" : repeat (text " *")) ds)
                  $$ text " */"

sizeInBrackets :: Type -> Doc
sizeInBrackets (NativeArray l t) = brackets (maybe empty (text . show) l) <> sizeInBrackets t
sizeInBrackets _                 = empty

stmt :: Doc -> Doc
stmt = (<>semi)

block :: Doc -> Doc
block d = lbrace $+$ nest 2 d $+$ rbrace

newPlace :: PrintEnv -> Place -> PrintEnv
newPlace env plc = env {place = plc}

