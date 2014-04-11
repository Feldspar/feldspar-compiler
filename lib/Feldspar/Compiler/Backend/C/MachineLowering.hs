{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
module Feldspar.Compiler.Backend.C.MachineLowering (rename) where

import qualified Data.Map as M

import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Backend.C.Platforms (extend, c99, tic64x)

-- | External interface for renaming.
rename :: Options -> Module () -> Module ()
rename opts m = rename' x m
  where x = getPlatformRenames (name $ platform opts)

-- | Internal interface for renaming.
rename' :: M.Map String [(Which, Destination)] -> Module () -> Module ()
rename' m (Module ents) = Module $ map (renameEnt m) ents

-- | Rename entities.
renameEnt :: M.Map String [(Which, Destination)] -> Entity () -> Entity ()
renameEnt m p@Proc{..}
  | Just body <- procBody = p { procBody = Just $ renameBlock m body }
renameEnt _ e             = e

-- | Rename blocks.
renameBlock :: M.Map String [(Which, Destination)] -> Block () -> Block ()
renameBlock m (Block vs p) = Block (map (renameDecl m) vs) (renameProg m p)

-- | Rename declarations.
renameDecl :: M.Map String [(Which, Destination)] -> Declaration () -> Declaration ()
renameDecl m (Declaration v (Just e)) = Declaration v (Just $ renameExp m e)
renameDecl _ d                        = d

-- | Rename programs.
renameProg :: M.Map String [(Which, Destination)] -> Program () -> Program ()
renameProg _ e@Empty              = e
renameProg _ c@Comment{}          = c
renameProg m (Assign lhs rhs)     = Assign (renameExp m lhs) (renameExp m rhs)
renameProg m (ProcedureCall n ps) = ProcedureCall n (map (renameParam m) ps)
renameProg m (Sequence ps)        = Sequence $ map (renameProg m) ps
renameProg m (Switch scrut alts)
   = Switch (renameExp m scrut) (map (renameAlt m) alts)
renameProg m (SeqLoop cond calc block)
  = SeqLoop (renameExp m cond) (renameBlock m calc) (renameBlock m block)
renameProg m (ParLoop p v e1 e2 b)
  = ParLoop p v (renameExp m e1) (renameExp m e2) (renameBlock m b)
renameProg m (BlockProgram b)     = BlockProgram $ renameBlock m b

-- | Rename expressions.
renameExp :: M.Map String [(Which, Destination)] -> Expression () -> Expression ()
renameExp _ v@VarExpr{}         = v
renameExp m (ArrayElem e1 e2)   = ArrayElem (renameExp m e1) (renameExp m e2)
renameExp m (StructField e s)   = StructField (renameExp m e) s
renameExp _ c@ConstExpr{}       = c
renameExp m (FunctionCall f es)
  = FunctionCall (renameFun m (typeof $ head es) f) $ map (renameExp m) es
renameExp m (Cast t e)          = Cast t $ renameExp m e
renameExp m (AddrOf e)          = AddrOf $ renameExp m e
renameExp _ s@SizeOf{}          = s
renameExp m (Deref e)           = Deref $ renameExp m e

-- | Rename parameters.
renameParam :: M.Map String [(Which, Destination)] -> ActualParameter ()
            -> ActualParameter ()
renameParam m (ValueParameter e) = ValueParameter $ renameExp m e
renameParam _ p                  = p

-- | Rename switch alternatives.
renameAlt :: M.Map String [(Which, Destination)] -> (Pattern (), Block ())
          -> (Pattern (), Block ())
renameAlt m (p, b) = (p, renameBlock m b)

-- | Renames functions that should be renamed. Identity function on others.
renameFun :: M.Map String [(Which, Destination)] -> Type -> Function -> Function
renameFun m argtype f@(Function name t p)
  | Just ps <- M.lookup name m
  , Just s <- findFun name argtype ps t = Function s t p
  | otherwise                           = f

-- | Finds the new name of the function, if any.
findFun :: String -> Type -> [(Which, Destination)] -> Type -> Maybe String
findFun name argtype m tp = go m
  where go []                          = Nothing
        go ((Only p, s):_) | true p tp = Just (newName name argtype tp s)
        go ((All, s):_)                = Just (newName name argtype tp s)
        go (_:t)                       = go t

-- | Returns a new name according to specification.
newName :: String -> Type -> Type -> Destination -> String
newName _    _       _  (Name s)                   = s
newName name _       tp (Extend FunType p)         = extend p name tp
newName name argtype _  (Extend ArgType p)         = extend p name argtype
newName _    _       tp (ExtendRename FunType p s) = extend p s tp
newName _    argtype _  (ExtendRename ArgType p s) = extend p s argtype

-- | Tells whether a predicate holds for a type.
true :: Predicate -> Type -> Bool
true Complex    t             = isComplex t
true Float      t             = isFloat t
true Signed32   t
  | Just 32 <- intWidth t
  , Just True <- intSigned t  = True
  | otherwise                 = False
true Unsigned32 t
  | Just 32 <- intWidth t
  , Just False <- intSigned t = True
  | otherwise                 = False

-- A rename is the name of the function to be renamed coupled with a
-- list of preconditions for renaming to happen and a the destination
-- name if the precondition is held. First match is executed and the
-- destination name becomes whatever the template specifies.

-- | C99 renaming list.
c99list :: [Rename]
c99list =
  [ ("/=",            [ (All, Name "!=")])
  , ("not",           [ (All, Name "!")])
  , ("quot",          [ (All, Name "/")])
  , ("rem",           [ (All, Name "%")])
  , (".&.",           [ (All, Name "&")])
  , (".|.",           [ (All, Name "|")])
  , ("xor",           [ (All, Name "^")])
  , ("complement",    [ (All, Name "~")])
  , ("shiftL",        [ (All, Name "<<")])
  , ("shiftLU",       [ (All, Name "<<")])
  , ("shiftR",        [ (All, Name ">>")])
  , ("shiftRU",       [ (All, Name ">>")])
  , ("creal",         [ (All, Name "crealf")])
  , ("cimag",         [ (All, Name "cimagf")])
  , ("conjugate",     [ (All, Name "conjf")])
  , ("magnitude",     [ (All, Name "cabsf")])
  , ("phase",         [ (All, Name "cargf")])
  , ("atan2",         [ (Only Complex, Name "atan2f") ])
  ] ++
  (map mkC99TrigRule ["exp", "sqrt", "log", "**", "sin", "tan", "cos", "asin"
                     , "atan", "acos", "sinh", "tanh", "cosh", "asinh", "atanh"
                     , "acosh"]) ++
  -- Extend these functions based on the function type.
  (map (mkC99ExtendRule FunType) [ "abs", "signum", "logBase", "setBit", "clearBit"
                                 , "complementBit", "rotateL", "rotateR"
                                 , "reverseBits" ]) ++
  -- Extend these functions based on the argument type.
  (map (mkC99ExtendRule ArgType) [ "testBit", "bitScan", "bitCount", "complex"
                                 , "mkPolar", "cis"])

-- | Make C99 extend rule.
mkC99ExtendRule :: WhichType -> String -> Rename
mkC99ExtendRule t s = (s, [ (All, Extend t c99) ])

-- | Make C99 trig rule.
mkC99TrigRule :: String -> Rename
mkC99TrigRule s = (s, [ (Only Complex, Name ('c':s')), (All, Name s') ])
  where s' = s ++ "f"

-- | Tic64x renaming list.
tic64xlist :: [Rename]
tic64xlist =
  [ ("==",          [ (Only Complex, ExtendRename ArgType tic64x "!=") ])
  , ("abs",         [ (Only Float, Name "_fabs"), (Only Signed32, Name "_abs") ])
  , ("+",           [ (Only Complex, ExtendRename ArgType tic64x "add") ])
  , ("-",           [ (Only Complex, ExtendRename ArgType tic64x "sub") ])
  , ("*",           [ (Only Complex, ExtendRename ArgType tic64x "mult") ])
  , ("/",           [ (Only Complex, ExtendRename ArgType tic64x "div") ])
  ] ++
  (map mkTic64xComplexRule ["exp", "sqrt", "log", "sin", "tan", "cos", "asin"
                           ,"atan", "acos", "sinh", "tanh", "cosh", "asinh"
                           ,"atanh","acosh","creal","cimag", "conjugate"
                           ,"magnitude","phase", "logBase"]) ++
  [ ("**",          [ (Only Complex, ExtendRename ArgType tic64x "cpow") ])
  , ("rotateL",     [ (Only Unsigned32, Name "_rotl") ])
  , ("reverseBits", [ (Only Unsigned32, Name "_bitr") ])
  ]

-- | Create Tic64x rule for complex type.
mkTic64xComplexRule :: String -> Rename
mkTic64xComplexRule s = (s, [ (Only Complex, Extend ArgType tic64x) ] )

-- | Returns the platform renames based on the platform name.
getPlatformRenames :: String -> M.Map String [(Which, Destination)]
getPlatformRenames "tic64x"       = M.fromList (tic64xlist ++ c99list)
getPlatformRenames s
  | s `elem` ["c99", "c99OpenMp"] = M.fromList c99list
  | otherwise                     = M.fromList []
