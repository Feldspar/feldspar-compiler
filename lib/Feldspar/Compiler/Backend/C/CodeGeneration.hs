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
    , place :: Place
    }
  deriving Show

compToCWithInfos :: Options -> Module () -> CompToCCoreResult ()
compToCWithInfos opts procedure =
  CompToCCoreResult { sourceCode  = render $ cgen (PEnv opts Declaration_pl) procedure
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
    cgen env ProcDef{..}   = text "void"     <+> text procName       <>  parens (cgenList (newPlace env MainParameter_pl) $ inParams ++ outParams) $$ block env (cgen env procBody)
    cgen env ProcDecl{..}  = text "void"     <+> text procName       <>  parens (cgenList (newPlace env MainParameter_pl) $ inParams ++ outParams) <> semi
    cgen env ValueDef{..}  = cgen env valVar <+> equals              <+> cgen (newPlace env ValueNeed_pl) valValue <> semi

    cgenList env = vcat . punctuate (text "\n") . map (cgen env)

instance CodeGen (StructMember ())
  where
    cgen env StructMember{..} = cgen env structMemberType <+> text structMemberName <> sizeInBrackets structMemberType <> semi

    cgenList env = vcat . map (cgen env)

instance CodeGen (Block ())
  where
    cgen env Block{..} = sep [ cgenList (newPlace env Declaration_pl) locals
                            , if (null locals) then empty else text ""
                            , cgen env blockBody
                            ]
    cgenList env = sep . punctuate (text "\n") . map (cgen env)

instance CodeGen (Declaration ())
  where
    cgen env Declaration{..} = cgen (newPlace env Declaration_pl) declVar <+> nest (nestSize $ options env) init <> semi
      where
        init = case (initVal, varType declVar) of
                 (Just i, _)           -> equals <+> cgen (newPlace env ValueNeed_pl) i
                 (_     , ArrayType{}) -> equals <+> braces (int 0)
                 _                     -> empty
    cgenList env = vcat . map (cgen env)

instance CodeGen (Program ())
  where
    cgen env Empty = empty
    cgen env Comment{..}
      | isBlockComment = blockComment $ map text $ lines commentValue
      | otherwise      = text "//" <+> text commentValue
    cgen env Assign{..} = cgen env' lhs <+> equals <+> nest (nestSize $ options env) (cgen env' rhs) <> semi
      where
        env' = newPlace env ValueNeed_pl
    cgen env ProcedureCall{..} = stmt $ call (text procCallName) (map (cgen env) procCallParams)
    cgen env Sequence{..} = cgenList env sequenceProgs
    cgen env Branch{..} =  text "if" <+> parens (cgen (newPlace env ValueNeed_pl) branchCond)
                       $$ block env (cgen env thenBlock)
                       $$ text "else"
                       $$ block env (cgen env elseBlock)
    cgen env Switch{..} =  text "switch" <+> parens (cgen env scrutinee)
                       $$ block env (vcat [ cgen env p $$ nest (nestSize $ options env) (cgen env b) | (p, b) <- alts])
    cgen env SeqLoop{..} =  cgen env sLoopCondCalc
                        $$ text "while" <+> parens (cgen (newPlace env ValueNeed_pl) sLoopCond)
                        $$ block env (cgen env sLoopBlock $+$ cgen env sLoopCondCalc)
    cgen env ParLoop{..} =  text "for" <+> parens (sep $ map (nest 4) $ punctuate semi [init, guard, next])
                        $$ block env (cgen env pLoopBlock)
      where
        ixd   = cgen envd pLoopCounter
        ixv   = cgen envv pLoopCounter
        init  = ixd <+> equals    <+> int 0
        guard = ixv <+> char '<'  <+> cgen envv pLoopBound
        next  = ixv <+> text "+=" <+> int pLoopStep
        envd  = newPlace env Declaration_pl
        envv  = newPlace env ValueNeed_pl

    cgen env BlockProgram{..} = block env (cgen env blockProgram)

    cgenList env = vcat . map (cgen env)

instance CodeGen (Pattern ())
  where
    cgen env PatDefault = text "default" <> colon
    cgen env (Pat c)    = cgen env c <> colon

    cgenList env = vcat . map (cgen env)

instance CodeGen (ActualParameter ())
  where
    cgen env (In VarExpr{..})
      | ArrayType{}   <- typeof var  = cgen (newPlace env AddressNeed_pl) var
      | StructType{}  <- typeof var  = cgen (newPlace env AddressNeed_pl) var
    cgen env In{..}                  = cgen (newPlace env FunctionCallIn_pl) inParam
    cgen env Out{..}                 = cgen (newPlace env AddressNeed_pl) outParam
    cgen env TypeParameter{..}       = cgen env typeParam
    cgen env FunParameter{..}        = prefix <> text funParamName
      where prefix = if addressNeeded then text "&" else empty
    cgenList env = hsep . punctuate comma . map (cgen env)

instance CodeGen (Expression ())
  where
    cgen env VarExpr{..} = cgen env var
    cgen env e@ArrayElem{..}  =  prefix <> (text "at")
                             <> parens (hcat $ punctuate comma [ cgen env $ typeof e
                                                               , cgen (newPlace env AddressNeed_pl) array
                                                               , cgen (newPlace env ValueNeed_pl) arrayIndex
                                                               ])
      where
        prefix = case (place env, typeof e) of
                   (AddressNeed_pl, _) -> text "&"
                   (_, ArrayType _ _)  -> text "&" -- TODO the call site should set the place to AddressNeed_pl for Arrays
                   _                   -> empty
    cgen env e@NativeElem{..} = prefix <> cgen (newPlace env AddressNeed_pl) array <> brackets (cgen (newPlace env ValueNeed_pl) arrayIndex)
      where
        prefix = case (place env, typeof e) of
                   (AddressNeed_pl, _) -> text "&"
                   (_, ArrayType _ _)  -> text "&" -- TODO the call site should set the place to AddressNeed_pl for Arrays
                   _                   -> empty
    cgen env e@StructField{..} = prefix <> cgen (newPlace env ValueNeed_pl) struct <> char '.' <> text fieldName
      where
        prefix = case (place env, typeof e) of
                   (AddressNeed_pl, _) -> text "&"
                   (_, ArrayType _ _)  -> text "&" -- TODO the call site should set the place to AddressNeed_pl for Arrays
                   _                   -> empty
    cgen env ConstExpr{..} = cgen env constExpr
    cgen env FunctionCall{..} | funName function == "!"   = call (text "at") $ map (cgen (newPlace env FunctionCallIn_pl)) funCallParams
                             | funMode function == Infix
                             , [a,b] <- funCallParams    = parens (cgen env a <+> text (funName function) <+> cgen env b)
                             | otherwise                 = call (text $ funName function) $ map (cgen (newPlace env FunctionCallIn_pl)) funCallParams
    cgen env Cast{..} = parens $ parens (cgen env castType) <> parens (cgen env castExpr)
    cgen env SizeOf{..} = call (text "sizeof") [either (cgen env) (cgen env) sizeOf]

    cgenList env = sep . punctuate comma . map (cgen env)

instance CodeGen (Variable t)
  where
    cgen env Variable{..} = case place env of
        MainParameter_pl -> typ <+> (ref <> name <> size)
        Declaration_pl   -> typ <+> (ref <> name <> size)
        _                -> ref <> name
      where
        typ  = cgen env varType
        size = sizeInBrackets varType
        ref  = case (varType, place env, passByReference varType) of
                 (Pointer{}, MainParameter_pl,  _    ) -> text "*"
                 (Pointer{}, AddressNeed_pl,    _    ) -> empty
                 (Pointer{}, FunctionCallIn_pl, True ) -> empty
                 (Pointer{}, _,                 _    ) -> text "*" -- char '*'
                 (_,         AddressNeed_pl,    _    ) -> text "&" -- char '&'
                 (_,         FunctionCallIn_pl, True ) -> text "&" -- char '&'
                 _                               -> empty
        name = text varName
        passByReference ArrayType{}   = True
        passByReference StructType{}  = True
        passByReference NativeArray{} = True
        passByReference (Pointer t)   = passByReference t
        passByReference _             = False

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
        toC (Pointer t)                = toC t -- TODO: Callee handling this?
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

newPlace :: PrintEnv -> Place -> PrintEnv
newPlace env plc = env {place = plc}

