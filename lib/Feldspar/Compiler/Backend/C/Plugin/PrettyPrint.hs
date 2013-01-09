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

import Text.PrettyPrint

-- ===========================================================================
--  == DebugToC plugin
-- ===========================================================================

data DebugToC = DebugToC

data DebugToCSemanticInfo

data PrintEnv = PEnv
    { -- Platform, Place and Indentation
      options :: Options,
      place :: Place
     }
     deriving Show

instance Transformation DebugToC where
    type From DebugToC    = ()
    type To DebugToC      = () -- DebugToCSemanticInfo
    type Down DebugToC    = PrintEnv
    type Up DebugToC      = Doc
    type State DebugToC   = ()

instance Plugin DebugToC where
    type ExternalInfo DebugToC = (Options, Place)
    executePlugin DebugToC (opts, plc) procedure =
        result $ transform DebugToC ()
                           (PEnv {options = opts, place = plc})
                           procedure

instance Combine Doc where combine = (<>)
instance Default Doc where def = empty

compToCWithInfos :: Options -> Int -> Module () -> CompToCCoreResult () -- DebugToCSemanticInfo
compToCWithInfos opts line procedure =
  CompToCCoreResult {
    sourceCode      = render $ up res
  , endPosition     = (0,0) -- state res
  , debugModule     = procedure -- result res
  } where
    res = transform DebugToC ()
                    (PEnv {options = opts, place = Declaration_pl}) procedure

instance Transformable DebugToC Variable where
    transform _ pos (PEnv {..}) x@(Variable vname typ role) = Result (Variable vname typ role) pos $ text $ toC options place x

instance Transformable1 DebugToC [] Constant where
    transform1 = transform1' (sep . punctuate comma)

instance Transformable DebugToC Constant where
    transform t pos down cnst@(IntConst c _) = Result cnst pos $ maybe (integer c) id $ transformConst down cnst

    transform t pos down cnst@(FloatConst c) = Result cnst pos $ maybe (double c) id $ transformConst down cnst

    transform t pos down cnst@(BoolConst False) = Result cnst pos $ maybe (int 0) id $ transformConst down cnst

    transform t pos down cnst@(BoolConst True) = Result cnst pos $ maybe (int 1) id $ transformConst down cnst

    transform t pos PEnv{..} cnst@ComplexConst{}
        | Just f <- lookup (typeof cnst) $ values $ platform options
        = Result cnst pos $ text $ f cnst
    transform t pos down cnst@ComplexConst{..}
        = Result cnst pos $ text "complex" <> parens (sep $ punctuate comma [ up $ complexTransform t down realPartComplexValue
                                                                            , up $ complexTransform t down imagPartComplexValue
                                                                            ]
                                                     )

transformConst :: PrintEnv -> Constant () -> Maybe Doc 
transformConst PEnv{..} cnst = do
  f <- lookup (typeof cnst) $ values $ platform options
  return $ text $ f cnst

instance Transformable DebugToC ActualParameter where
    transform t pos down act@(In (VarExpr var))
      | StructType{} <- typeof var = transformActParam t pos down act AddressNeed_pl
      | ArrayType{}  <- typeof var = transformActParam t pos down act AddressNeed_pl
    transform t pos down act@In{}            = transformActParam t pos down act FunctionCallIn_pl
    transform t pos down act@Out{}           = transformActParam t pos down act AddressNeed_pl
    transform t pos down act@TypeParameter{} = transformActParam t pos down act MainParameter_pl
    transform t pos down act@FunParameter{}  = transformActParam t pos down act FunctionCallIn_pl

transformActParam _ pos PEnv{..} par@(TypeParameter typ mode) _ = Result par pos
    $ text $ showType options Value (place mode) typ NoRestrict
  where
    place Auto   = MainParameter_pl
    place Scalar = Declaration_pl

transformActParam _ pos _ par@(FunParameter n k addr) _ = Result par pos $
    if addr then char '&' <> text n
            else text n

transformActParam t pos down act paramType
    = Result act pos $ up $ transform t pos (newPlace down paramType) (getParam act)
  where
    getParam (In param)  = param
    getParam (Out param) = param

instance Transformable1 DebugToC [] Expression where
    transform1 = transform1' (sep . punctuate comma)

call :: Doc -> [Doc] -> Doc
call fn args = fn <> parens (sep $ punctuate comma args)

instance Transformable DebugToC Expression where
    transform t pos down (VarExpr val) = Result (VarExpr val) pos $ up $ transform t pos down val

    transform t pos down@PEnv{..} exp@(ArrayElem n index) = Result exp pos
        $ prefix <> call (text "at") [ up $ transform t pos (newPlace down AddressNeed_pl) n
                                     , text $ toC options place $ typeof exp
                                     , up $ transform t pos (newPlace down ValueNeed_pl) index
                                     ]
      where
        prefix = case (place, typeof exp) of
           (AddressNeed_pl, _) -> char '&'
           (_, ArrayType _ _)  -> char '&' -- TODO the call site should set the place to AddressNeed_pl for Arrays
           _                   -> empty

    transform t pos down@(PEnv {..}) e@(NativeElem n index) = Result e pos
        $ prefix <> hcat [ up $ transform t pos down n
                         , brackets (up $ transform t pos (newPlace down ValueNeed_pl) index)
                         ]
      where
        prefix = case (place, typeof e) of
           (AddressNeed_pl, _) -> char '&'
           (_, ArrayType _ _)  -> char '&' -- TODO the call site should set the place to AddressNeed_pl for Arrays
           _                   -> empty

    transform t pos down@PEnv{..} expr@(StructField e field) = Result expr pos
        $ prefix <> hcat [ up $ transform t pos (newPlace down ValueNeed_pl) e
                         , char '.'
                         , text field
                         ]
      where
        prefix = case (place, typeof expr) of
           (AddressNeed_pl, _) -> char '&'
           (_, ArrayType _ _)  -> char '&'
           _                   -> empty

    transform t pos down e@(ConstExpr val) = Result e pos $ up $ transform t pos down val

    transform t pos down fc@(FunctionCall f args@[_,_])
        | funName f == "!"
        = Result fc pos $ call (text "at") $ map (up . transform t pos down) args

    transform t pos down fc@(FunctionCall f [a,b])
        | funMode f == Infix
        = Result fc pos $ parens $ sep [ up (transform t pos down a) <+> text (funName f)
                                       , up $ transform t pos down b
                                       ]

    transform t pos down fc@(FunctionCall f args)
        = Result fc pos $ call (text $ funName f) $ map (up . transform t pos down) args

    transform t pos down@PEnv{..} exp@(Cast typ e)
        = Result exp pos $ parens $  parens (text $ toC options place typ)
                                  <> parens (up $ transform t pos down e)

    transform t pos down@PEnv{..} exp@(SizeOf x)
      = Result exp pos $ call (text "sizeof") [either (text . toC options place) (up . transform t pos down) x]

instance Transformable DebugToC Module where
    transform t pos down mod@(Module defList) = Result mod pos
      $ up1 $ transform1 t pos down defList 

instance Transformable1 DebugToC [] Variable where
    transform1 = transform1' (sep . punctuate comma)

instance Transformable1 DebugToC [] StructMember where
    transform1 = transform1' vcat

instance Transformable1 DebugToC [] Entity where
    transform1 = transform1' (vcat . punctuate (text "\n"))

instance Transformable DebugToC Entity where
    transform t pos down@PEnv{..} ent@(StructDef n members)
      = Result ent pos $   text "struct" <+> text n
                       $+$ block (up1 $ transform1 t pos down members)
                       <>  semi

    transform _ pos PEnv{..} ent@(TypeDef typ n)
      = Result ent pos $   text "typedef"
                       <+> text (showType options Value place typ NoRestrict)
                       <+> text n
                       <>  semi

    transform t pos down@PEnv{..} ent@(ProcDef n _ inp outp body) =
      Result ent pos $  text "void" <+> text n
                     <> parens (up1 $ transform1 t pos down $ inp ++ outp)
                     $$ block (up $ transform t pos down body)

    transform t pos down@PEnv{..} ent@(ProcDecl n _ inp outp) =
      Result ent pos $  text "void" <+> text n
                     <> parens (up1 $ transform1 t pos down $ inp ++ outp)
                     <> semi

blockComment :: [Doc] -> Doc
blockComment ds = vcat (zipWith (<+>) (text "/*" : repeat (text " *")) ds)
                  $$ text " */"

instance Transformable DebugToC StructMember where
    transform _ pos PEnv{..} sm@(StructMember str typ) = Result sm pos
      $ stmt $ case typ of
                 ArrayType{} -> text (showVariable options place Value typ str)
                 _           -> text (toC options place typ) <+> text str

instance Transformable1 DebugToC [] Declaration where
    transform1 = transform1' (vcat . map stmt)

instance Transformable DebugToC Block where
    transform t pos down@(PEnv {..}) (Block locs body) = Result (Block locs body) pos
      $ sep $ [ up1 $ transform1 t pos (newPlace down Declaration_pl) locs
              , if (null locs) then empty else text ""
              , up  $ transform t pos down body
              ]

instance Transformable DebugToC Declaration where
    transform t pos down (Declaration dv Nothing) = Result (Declaration dv Nothing) pos
      $ sep [ up $ transform t pos (newPlace down Declaration_pl) dv
            , nest 2 $ case varType dv of
                         ArrayType{} -> equals <+> braces (int 0)
                         _           -> empty
            ]

    transform t pos down (Declaration dv (Just e)) = Result (Declaration dv (Just e)) pos
      $ sep [ up $ transform t pos (newPlace down Declaration_pl) dv
            , text " = "
            , up $ transform t pos (newPlace down ValueNeed_pl) e
            ]

instance Transformable1 DebugToC [] ActualParameter where
    transform1 = transform1' (sep . punctuate comma)

instance Transformable1 DebugToC [] Program where
    transform1 = transform1' vcat

stmt :: Doc -> Doc
stmt = (<>semi)

block :: Doc -> Doc
block d = lbrace $+$ nest 2 d $+$ rbrace

instance Transformable DebugToC Program where
    transform _ pos _ Empty = Result Empty pos empty

    transform _ pos down prg@(Comment True comment)
      = Result prg pos $ blockComment $ map text $ lines comment

    transform _ pos down prg@(Comment False comment)
      = Result prg pos $ text "//" <+> text comment

    transform t pos down@PEnv{..} prg@(Assign lh rh) = Result prg pos
      $ sep [ up (transform t pos (newPlace down ValueNeed_pl) lh) <+> equals
            , nest 2 $ up $ transform t pos (newPlace down ValueNeed_pl) rh
            ] <> semi

    transform t pos down@PEnv{..} prg@(ProcedureCall n k param)
      = Result prg pos $ stmt $ call (text n) (map (up . transform t pos down) param)

    transform t pos down prg@(Sequence prog)
      = Result prg pos $ up1 $ transform1 t pos down prog

    transform t pos down (Branch con tPrg ePrg) = Result (Branch con tPrg ePrg) pos
      $  text "if" <> parens (up $ transform t pos (newPlace down ValueNeed_pl) con)
      $$ block (up $ transform t pos down tPrg)
      $$ text "else" 
      $$ block (up $ transform t pos down ePrg)

    transform t pos down (Switch scrut alts) = error "TODO: PrettyPrint for switch"

    transform t pos down (SeqLoop con conPrg blockPrg) = Result (SeqLoop con conPrg blockPrg) pos
      $ block $ sep [ up $ transform t pos down conPrg
                    , text "while" <> parens (up $ transform t pos (newPlace down ValueNeed_pl) con)
                    , block $ sep [ up $ transform t pos down blockPrg
                                  , up $ transform t pos down (blockBody conPrg)
                                  ]
                    ]

    transform t pos down (ParLoop count bound step prog) = Result (ParLoop count bound step prog) pos
        $ sep [ text "for" <> parens (sep $ map (nest 4) $ punctuate semi [init, guard, next])
              , block $ up $ transform t pos down prog
              ]
      where
        ix    = up $ transform t pos (newPlace down ValueNeed_pl) count
        init  = ix <+> equals    <+> int 0
        guard = ix <+> char '<'  <+> (up $ transform t pos (newPlace down ValueNeed_pl) bound)
        next  = ix <+> text "+=" <+> int step

    transform t pos down prg@(BlockProgram prog)
      = Result prg pos $ block $ up $ transform t pos down prog

transform1' _   _ pos _    [] = Result1 [] pos empty
transform1' f t pos down xs = Result1 xs pos $ f $ map (up . transform t pos down) xs

complexTransform t down d = transform t () down d

newPlace :: PrintEnv -> Place -> PrintEnv
newPlace env plc = env {place = plc}

