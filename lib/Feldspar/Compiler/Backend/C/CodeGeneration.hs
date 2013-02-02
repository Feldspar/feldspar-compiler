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

codeGenerationError :: ErrorClass -> String -> a
codeGenerationError = handleError "CodeGeneration"

data PrintEnv = PEnv
    { options :: Options
    }
  deriving Show

compToCWithInfos :: Options -> Module () -> CompToCCoreResult ()
compToCWithInfos opts procedure =
  CompToCCoreResult { sourceCode  = render $ cgen (PEnv opts) procedure
                    , debugModule = procedure
                    }

class CodeGen a
  where
    cgen     :: PrintEnv -> a -> Doc
    cgenList :: PrintEnv -> [a] -> Doc

instance CodeGen (Module ())
  where
    cgen env Module{..} = cgenList env entities
    cgenList env = sep . map (cgen env)

instance CodeGen (Entity ())
  where
    cgen env StructDef{..} = text "struct"   <+> text structName     $+$ block env (cgenList env structMembers) <> semi
    cgen env TypeDef{..}   = text "typedef"  <+> cgen env actualType <+> text typeName <> semi
    cgen env ProcDef{..}   = text "void"     <+> text procName       <>  parens (pvars env $ inParams ++ outParams) $$ block env (cgen env procBody)
    cgen env ProcDecl{..}  = text "void"     <+> text procName       <>  parens (pvars env $ inParams ++ outParams) <> semi
    cgen env ValueDef{..}  = cgen env valVar <+> equals              <+> cgen env valValue <> semi

    cgenList env = vcat . punctuate (text "\n") . map (cgen env)

instance CodeGen (StructMember ())
  where
    cgen env StructMember{..} = cgen env structMemberType <+> text structMemberName <> sizeInBrackets structMemberType <> semi

    cgenList env = vcat . map (cgen env)

instance CodeGen (Block ())
  where
    cgen env Block{..} = sep [ cgenList env locals
                            , if null locals then empty else text ""
                            , cgen env blockBody
                            ]
    cgenList env = sep . punctuate (text "\n") . map (cgen env)

instance CodeGen (Declaration ())
  where
    cgen env Declaration{..} = pvar env declVar <+> nest (nestSize $ options env) init <> semi
      where
        init = case (initVal, varType declVar) of
                 (Just i, _)           -> equals <+> cgen env i
                 (_     , Pointer{})   -> equals <+> text "NULL"
                 (_     , ArrayType{}) -> equals <+> braces (int 0)
                 _                     -> empty
    cgenList env = vcat . map (cgen env)

instance CodeGen (Program ())
  where
    cgen env Empty = empty
    cgen env Comment{..}
      | isBlockComment = blockComment $ map text $ lines commentValue
      | otherwise      = text "//" <+> text commentValue
    cgen env Assign{..} = cgen env lhs <+> equals <+> nest (nestSize $ options env) (cgen env rhs) <> semi
    cgen env ProcedureCall{..} = stmt $ call (text procCallName) (map (cgen env) procCallParams)
    cgen env Sequence{..} = cgenList env sequenceProgs
    cgen env Branch{..} =  text "if" <+> parens (cgen env branchCond)
                       $$ block env (cgen env thenBlock)
                       $$ text "else"
                       $$ block env (cgen env elseBlock)
    cgen env Switch{..} =  text "switch" <+> parens (cgen env scrutinee)
                       $$ block env (vcat [ cgen env p $$ nest (nestSize $ options env) (cgen env b) | (p, b) <- alts])
    cgen env SeqLoop{..} =  cgen env sLoopCondCalc
                        $$ text "while" <+> parens (cgen env sLoopCond)
                        $$ block env (cgen env sLoopBlock $+$ cgen env sLoopCondCalc)
    cgen env ParLoop{..} =  text "for" <+> parens (sep $ map (nest 4) $ punctuate semi [init, guard, next])
                        $$ block env (cgen env pLoopBlock)
      where
        ixd   = pvar env pLoopCounter
        ixv   = cgen env  pLoopCounter
        init  = ixd <+> equals    <+> int 0
        guard = ixv <+> char '<'  <+> cgen env pLoopBound
        next  = ixv <+> text "+=" <+> int pLoopStep

    cgen env BlockProgram{..} = block env (cgen env blockProgram)

    cgenList env = vcat . map (cgen env)

instance CodeGen (Pattern ())
  where
    cgen env PatDefault = text "default" <> colon
    cgen env (Pat c)    = cgen env c <> colon

    cgenList env = vcat . map (cgen env)

instance CodeGen (ActualParameter ())
  where
    cgen env In{..}                  = cgen env inParam
    cgen env Out{..}                 = cgen env outParam
    cgen env TypeParameter{..}       = cgen env typeParam
    cgen env FunParameter{..}        = prefix <> text funParamName
      where prefix = if addressNeeded then text "&" else empty
    cgenList env = hsep . punctuate comma . map (cgen env)

instance CodeGen (Expression ())
  where
    cgen env VarExpr{..} = cgen env var
    cgen env e@ArrayElem{..}  =  text "at"
                             <> parens (hcat $ punctuate comma [ cgen env $ typeof e
                                                               , cgen env array
                                                               , cgen env arrayIndex
                                                               ])
    cgen env e@NativeElem{..} = cgen env array <> brackets (cgen env arrayIndex)
    cgen env e@StructField{..} = parens (cgen env struct) <> char '.' <> text fieldName
    cgen env ConstExpr{..} = cgen env constExpr
    cgen env FunctionCall{..} | funName function == "!"   = call (text "at") $ map (cgen env) funCallParams
                             | funMode function == Infix
                             , [a,b] <- funCallParams    = parens (cgen env a <+> text (funName function) <+> cgen env b)
                             | otherwise                 = call (text $ funName function) $ map (cgen env) funCallParams
    cgen env Cast{..} = parens $ parens (cgen env castType) <> parens (cgen env castExpr)
    cgen env AddrOf{..} | VarExpr v@(Variable{..}) <- addrExpr
                        , Pointer t <- typeof addrExpr = cgen env (v{varType = t})
                        | otherwise                      = prefix <> cgen env addrExpr
     where
       prefix = case typeof addrExpr of
                  Pointer{} -> empty
                  _         -> text "&"
    cgen env SizeOf{..} = call (text "sizeof") [either (cgen env) (cgen env) sizeOf]

    cgenList env = sep . punctuate comma . map (cgen env)

instance CodeGen (Variable t)
  where
    cgen env Variable{..} = prefix <> text varName
      where
        prefix = case varType of
                   Pointer{} -> text "*" -- char '*'
                   _         -> empty

    cgenList env = hsep . punctuate comma . map (cgen env)

instance CodeGen (Constant ())
  where
    cgen env cnst@(IntConst c _)    = maybe (integer c) text $ transformConst env cnst
    cgen env cnst@(FloatConst c)    = maybe (double c)  text $ transformConst env cnst
    cgen env cnst@(BoolConst False) = maybe (int 0)     text $ transformConst env cnst
    cgen env cnst@(BoolConst True)  = maybe (int 1)     text $ transformConst env cnst
    cgen env cnst@ComplexConst{..}  = maybe cmplxCnst   text $ transformConst env cnst
      where
        cmplxCnst = text "complex" <> parens (cgenList env [realPartComplexValue, imagPartComplexValue])
    cgenList env = sep . punctuate comma . map (cgen env)

transformConst :: PrintEnv -> Constant () -> Maybe String 
transformConst PEnv{..} cnst = do
    f <- lookup (typeof cnst) $ values $ platform options
    return $ f cnst

instance CodeGen Type
  where
    cgen env = toC
      where
        toC VoidType                   = text "void"
        toC ArrayType{}                = text "struct array"
        toC IVarType{}                 = text "struct ivar"
        toC (UserType u)               = text u
        toC (StructType n _)           = text "struct" <+> text n
        toC (NativeArray _ t)          = toC t
        toC (Pointer t)                = toC t <+> text "*"
        toC t | Just s <- lookup t pts = text s
        toC t = codeGenerationError InternalError
              $ unwords ["Unhandled type in platform ", name pfm,  ": ", show t]
        pfm = platform $ options env
        pts = types pfm

    cgenList env = sep . map (cgen env)

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

block :: PrintEnv -> Doc -> Doc
block env d = lbrace $+$ nest (nestSize $ options env) d $+$ rbrace

pvar :: PrintEnv -> Variable t -> Doc
pvar env Variable{..} = typ <+> (name <> size)
  where
    typ  = cgen env varType
    size = sizeInBrackets varType
    name = text varName

pvars :: PrintEnv -> [Variable t] -> Doc
pvars env = hsep . punctuate comma . map (pvar env)
