module Feldspar.Compiler.Imperative.ExternalProgram (parseFile, massageInput) where

import Feldspar.Lattice (universal)
import Feldspar.Compiler.Imperative.FromCore.Interpretation (decodeType)
import qualified Feldspar.Compiler.Imperative.Representation as R
import Feldspar.Compiler.Imperative.Representation hiding (
  Block, Switch, Assign, Cast, IntConst, FloatConst, DoubleConst, Type,
  Deref, AddrOf, Unsigned, Signed)
import Feldspar.Compiler.Imperative.Frontend (litB, toBlock)

import qualified Data.ByteString.Char8 as B
import qualified Language.C.Parser as P
import qualified Language.C.Parser.Tokens as T
import Language.C.Syntax
import Data.List (intersperse, mapAccumL)
import Data.Loc
import Data.Maybe

-- Necessary for now.
import Debug.Trace

-- The parser is quite general and handles ObjC and has support for
-- AntiQuotations. We are interested in parsing "External Program"
-- which grammatically is a subset of C, so most functions in this
-- file match on some constructors and end with a general error case
-- that tells which function it is and shows the input it got.
--
-- The intention is that these errors should only capture the
-- constructors that are outside the C language, and valid "External
-- Program" programs will never see them. The second intention is that
-- these error messages will be unique and easy to pinpoint in the case
-- that bugs do occur.

parseFile :: FilePath -> B.ByteString -> [Entity ()] -> Maybe (Module ())
parseFile filename s headerDefs = do
  case P.parse [C99] builtin_types P.parseUnit s' (startPos filename) of
      Left err -> Nothing
      Right defs -> Just (Module $ fhDefs ++ toProgram headerDefs defs)
   where s' = massageInput s
         fhDefs = filter isStructDef headerDefs
         isStructDef StructDef{} = True
         isStructDef _           = False

-- A list of built in types. We need the regular C99 types in this list, or
-- the parser will die with a mysterious parse error.
builtin_types :: [String]
builtin_types = [ "uint32_t", "uint16_t", "uint8_t"
                , "int32_t", "int16_t", "int8_t"]

toProgram :: [Entity ()] -> [Definition] -> [Entity ()]
toProgram hDefs defs = defsToProgram (emptyEnv (patchHdefs hDefs)) defs

defsToProgram :: TPEnv -> [Definition] -> [Entity ()]
defsToProgram env defs = snd $ mapAccumL defToProgram env defs

defToProgram :: TPEnv -> Definition -> (TPEnv, Entity ())
defToProgram env (FuncDef func _) = funcToProgram env func
defToProgram env (DecDef (InitGroup ds attr is@[(Init n Array{} Nothing (Just (CompoundInitializer ins _)) _ _)] _) _)
  = valueToProgram env ds is n ins'
   where ins' = map snd ins
defToProgram env (DecDef ig _) = initGroupToDeclaration env ig
defToProgram _ (EscDef s _) = error ("defToProgram: " ++ show s)
defToProgram _ e = error ("defToProgram: Unhandled construct: " ++ show e)

funcToProgram :: TPEnv -> Func -> (TPEnv, Entity ())
funcToProgram env (Func ds name decl (Params parms _ _) bis _)
  = (env', Proc (unId name) (init vs) [last vs] (Just bs))
   where (env', vs) = mapAccumL paramToVariable env parms
         bs = blockToBlock env' bis
funcToProgram _ e = error ("funcToProgram: Unhandled construct: " ++ show e)

valueToProgram :: TPEnv -> DeclSpec -> [Init] -> Id -> [Initializer]
               -> (TPEnv, Entity ())
valueToProgram env ds is n ins = (env', ValueDef (nameToVariable env' n) cs')
  where env' = initToNames env ds is
        cs = map (\(ExpInitializer (Const c _) _) -> constToConstant c) ins
        cs' = ArrayConst $ map (castConstant (declSpecToType env ds)) cs

paramToVariable :: TPEnv -> Param -> (TPEnv, R.Variable ())
paramToVariable env (Param (Just id) t p _) = (updateEnv env [v], v)
  where v = Variable (declToType (declSpecToType env t) p) (unId id)
paramToVariable _ e = error ("paramToVariable: Unhandled construct: " ++ show e)

blockToBlock :: TPEnv -> [BlockItem] -> R.Block ()
blockToBlock env bis = R.Block (concat ds) body
  where (env', ds) = mapAccumL blockDeclToDecl env decls
        body = Sequence $ blockItemsToProgram env' rest
        (decls, rest) = span isBlockDecl bis
        isBlockDecl BlockDecl{} = True
        isBlockDecl _           = False

blockDeclToDecl :: TPEnv -> BlockItem -> (TPEnv, [Declaration ()])
blockDeclToDecl env (BlockDecl ig) = (env', dv)
  where igv = vars $ initGroupToProgram (emptyEnv (headerDefs env)) ig
        env' = env { vars = (igv ++ vars env) } -- complete env
        dv = map (\(_, v) -> Declaration v Nothing) igv -- Program decl

blockItemsToProgram :: TPEnv -> [BlockItem] -> [Program ()]
blockItemsToProgram env is = snd $ mapAccumL blockItemToProgram env is

blockItemToProgram :: TPEnv -> BlockItem -> (TPEnv, Program ())
blockItemToProgram _ b@BlockDecl{}
  = error ("Declaration in the middle of a block: " ++ show b)
-- Ivar reconstruction stuff.
-- Change the name on ivar_get_nontask.
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "ivar_get_nontask" _) _) es _)) _))
  = (env, ProcedureCall "ivar_get" (tp:map ValueParameter es))
    where (FunctionCall (Function "ivar_get_nontask" _ _) es) = expToExpression env e
          tp = TypeParameter $ typeof (R.Deref (head es))
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "ivar_put" _) _) es _)) _))
  = (env, ProcedureCall s (tp:(map ValueParameter es)))
   where (FunctionCall (Function s _ _) es) = expToExpression env e
         tp = TypeParameter (typeof (R.Deref (last es)))
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "run2" _) _) es _)) _))
  = (env, ProcedureCall s [FunParameter e1,tp1,tp2])
   where (FunctionCall (Function s _ _) [VarExpr (Variable _ e1)]) = expToExpression env e
         tp2 = TypeParameter (IVarType fakeType)
         tp1 = TypeParameter $ lookup3 e1 (headerDefs env)
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "spawn2" _) _) es _)) _))
  = (env, ProcedureCall s es)
   where (FunctionCall (Function s _ _) [(VarExpr (Variable _ e1)),e2,e3]) = expToExpression env e
         es = [ FunParameter e1, TypeParameter (typeof e2), ValueParameter e2
              , TypeParameter (IVarType fakeType), ValueParameter e3]
-- Probably copyArray.
blockItemToProgram env (BlockStm (Exp (Just e@FnCall{}) _))
  = (env, ProcedureCall s (map ValueParameter es))
   where (FunctionCall (Function s _ _) es) = expToExpression env e
blockItemToProgram env (BlockStm (Exp (Just e) _)) = impureExpToProgram env e
blockItemToProgram env (BlockStm e) = (env, stmToProgram env e)
blockItemToProgram _ e = error ("blockItemsToProgram: Unhandled construct: " ++ show e)

initGroupToDeclaration :: TPEnv -> InitGroup -> (TPEnv, Entity ())
-- Struct definitions and similar.
initGroupToDeclaration env (InitGroup ds attr [] _) = (env', s)
  where env' = updateEnv2 env [] t
        t = declSpecToType env ds
        [s] = mkDef t
-- Function declarations
initGroupToDeclaration env (InitGroup ds attr [is@(Init n (Proto _ (Params ps _ _) _) _ _ _ _)] _)
  = initToFunDecl env ds is ps
initGroupToDeclaration _ e = error ("initGroupToDeclaration: " ++ show e)

initGroupToProgram :: TPEnv -> InitGroup -> TPEnv
-- Variable declarations
initGroupToProgram env (InitGroup ds attr is _) = initToNames env ds is
initGroupToProgram env (TypedefGroup ds attr ts _)
  = error "initGroupToProgram: hit TypedefGroup."
initGroupToProgram _ e
  = error ("initGroupToProgram: Unhandled construct: " ++ show e)

-- Contrived type to swallow break statements silently.
switchAltToProgram :: TPEnv -> BlockItem -> [(Pattern (), R.Block ())]
switchAltToProgram _ (BlockStm Break{}) = []
switchAltToProgram env (BlockStm (Case e s _))
  = [(Pat (expToExpression env e), toBlock (stmToProgram env s))]
switchAltToProgram env (BlockStm (Default stm _))
  = [(PatDefault, toBlock (stmToProgram env stm))]
switchAltToProgram _ e = error ("switchAltToProgram: " ++ show e)

stmToProgram :: TPEnv -> Stm -> Program ()
stmToProgram _ l@Label{} = error ("stmToProgram: Unexpected label: " ++ show l)
stmToProgram _ (Exp Nothing _) = error "Exp: Nothing?"
stmToProgram env (Exp (Just e) _) = snd $ impureExpToProgram env e
stmToProgram env (Block bis _) = Sequence (blockItemsToProgram env bis)
stmToProgram env (If e b1@(Block alt _) (Just b2@(Block alt' _)) _)
  = R.Switch cond [ (Pat $ litB True, toBlock $ stmToProgram env b1)
                  , (Pat $ litB False, toBlock $ stmToProgram env b2)]
    where cond = expToExpression env e
stmToProgram env (Switch e (Block alts _) _)
  = R.Switch (expToExpression env e) $ concatMap (switchAltToProgram env) alts
stmToProgram env (While e s _) = SeqLoop cond (toBlock Empty) (toBlock p)
  where cond = expToExpression env e
        p = stmToProgram env s
stmToProgram env (DoWhile s e _) = error "stmToProgram: No support for Do."
stmToProgram env (For (Left es) (Just (BinOp Lt name@Var{} v2 _))
                      (Just (Assign lhs AddAssign rhs _)) s _)
  = ParLoop False v' (expToExpression env' v2) (expToExpression env' rhs) body
    where env' = initGroupToProgram env es
          v' = varToVariable env' name
          body = toBlock $ stmToProgram env' s
stmToProgram _ Goto{} = error "stmToProgram: No support for goto."
stmToProgram _ Continue{} = error "stmToProgram: No support for continue."
stmToProgram _ Break{} = error "stmToProgram: Unexpected break."
stmToProgram _ Return{} = error "stmToProgram: Unexpected return."
stmToProgram env (Pragma s _) = error "Pragma not supported yet."
stmToProgram _ a@Asm{} = error ("stmToProgram: unexpected asm: " ++ show a)
stmToProgram _ e = error ("stmToProgram: Unhandled construct: " ++ show e)

impureExpToProgram :: TPEnv -> Exp -> (TPEnv, Program ())
-- Hook for padding incomplete struct array * type info.
impureExpToProgram env (Assign e@(Var (Id s _) _) JustAssign
                               f@(FnCall (Var (Id "initArray" _) _)
                                         [e1', (SizeofType e2' _), e3'] _) _)
  = (env', R.Assign (expToExpression env' e) (expToExpression env' f))
   where env' = fixupEnv env s $ typToType env e2'
impureExpToProgram env (Assign e@(Var (Id s _) _) JustAssign
                               f@(FnCall (Var (Id "setLength" _) _)
                                         [e1', (SizeofType e2' _), e3'] _) _)
  = (env', R.Assign (expToExpression env' e) (expToExpression env' f))
   where env' = fixupEnv env s $ typToType env e2'
impureExpToProgram env (Assign e1 JustAssign e2 _)
  = (env, R.Assign (expToExpression env e1) (expToExpression env e2))
impureExpToProgram _ e = error ("impureExpToProgram: " ++ show e)

varToVariable :: TPEnv -> Exp -> Variable ()
varToVariable env (Var name _) = nameToVariable env name

nameToVariable :: TPEnv -> Id -> Variable ()
nameToVariable env name
 | Just v <- lookup (unId name) (vars env) = v
 | take 4 (unId name) == "task" = Variable fakeType (unId name) -- fake tasks
 | otherwise = error ("varToVariable: Could not find: " ++ show name)

-- No concept of Bool constants in the C parser. They appear at expresion
-- positions in our context.
expToExpression :: TPEnv -> Exp -> Expression ()
expToExpression _ (Var n _)
  | unId n == "true" = litB True
  | unId n == "false" = litB False
expToExpression env v@Var{} = VarExpr $ varToVariable env v
expToExpression _ (Const c _) = ConstExpr (constToConstant c)
expToExpression env (BinOp op e1 e2 _) = opToFunction parms op
  where parms = map (expToExpression env) [e1, e2]
expToExpression env (UnOp op e _) = unOpToExp (expToExpression env e) op
expToExpression _ (Assign e1 JustAssign e2 _) = error "Assign unimplemented"
expToExpression _ a@Assign{} = error ("AssignOp unhandled: " ++ show a)
expToExpression _ PreInc{} = error "expToExpression: No support for preinc."
expToExpression _ PostInc{} = error "expToExpression: No support for postinc."
expToExpression _ PreDec{} = error "expToExpression: No support for predec."
expToExpression _ PostDec{} = error "expToExpression: No support for postdec."
expToExpression _ SizeofExp{} = error "expToExpression: No support for sizeof exp"
expToExpression env (SizeofType t _) = R.SizeOf $ typToType env t
expToExpression env (Cast t e _)
  = R.Cast (typToType env t) (expToExpression env e)
expToExpression env (Cond c e1 e2 _) = error "expToExpression: No support for conditional statements."
expToExpression env (Member e name _)
  = StructField (expToExpression env e) (unId name)
expToExpression _ PtrMember{} = error "expToExpression: No support for ptrmember."
expToExpression env (Index e1 e2 _)
  = ArrayElem (expToExpression env e1) (expToExpression env e2)
expToExpression env (FnCall (Var (Id "at" _) _) [e1, e2] _)
  = ArrayElem (expToExpression env e1) (expToExpression env e2)
expToExpression env (FnCall e es _)
  = FunctionCall (expToFunction env e) (map (expToExpression env) es)
expToExpression _ CudaCall{} = error "expToExpression: No support for CUDA."
expToExpression _ Seq{} = error "expToExpression: No support for seq."
expToExpression env (CompoundLit t ls _) = ConstExpr $ ArrayConst cs'
  where cs = map (\(_, ExpInitializer (Const c _) _) -> constToConstant c) ls
        cs' = map (castConstant (typToType env t)) cs
expToExpression _ StmExpr{} = error "expToExpression: No support for Stmexpr."
expToExpression _ BuiltinVaArg{} = error "expToExpression: varargs not supported."
expToExpression _ BlockLit{} = error "expToExpression: No support for blocklit."
expToExpression _ e = error ("expToExpression: Unhandled construct: " ++ show e)

opToFunction :: [Expression ()] -> BinOp -> Expression ()
opToFunction es op = FunctionCall (case opToString op of
                      Right s -> Function s t Infix
                      Left s -> Function s BoolType Infix) es
  where t = typeof (head es)

opToString :: BinOp -> Either String String
opToString Add = Right "+"
opToString Sub = Right "-"
opToString Mul = Right "*"
opToString Div = Right "/"
opToString Mod = Right "%"
opToString Eq = Left "=="
opToString Ne = Left "!="
opToString Lt = Left "<"
opToString Gt = Left ">"
opToString Le = Left "<="
opToString Ge = Left ">="
opToString Land = Left "&&"
opToString Lor = Left "||"
opToString And = Right "&"
opToString Or = Right "|"
opToString Xor = Right "^"
opToString Lsh = Right "<<"
opToString Rsh = Right ">>"

expToFunction :: TPEnv -> Exp -> R.Function
expToFunction env (Var name _)
  | Just v <- lookup (unId name) builtins
  = Function (varName v) (varType v) Prefix
  | otherwise = Function (unId name) (UserType "Unknown") Prefix

-- Signed integers are the default for literals, but that is not always
-- convenient. Fix things up afterwards instead.
castConstant :: R.Type -> R.Constant () -> R.Constant ()
castConstant t (R.IntConst i _) = R.IntConst i t
castConstant _ c = error ("castConstant: Unexpected argument: " ++ show c)

constToConstant :: Const -> Constant ()
constToConstant (IntConst s sgn i _)
  = R.IntConst i (NumType (signToSign sgn) S32)
constToConstant (LongIntConst s sgn i _) = R.IntConst i fakeType
constToConstant (LongLongIntConst s sgn i _) = R.IntConst i fakeType
constToConstant (FloatConst s r _) = R.FloatConst (fromRational r)
constToConstant (DoubleConst s r _) = R.DoubleConst (fromRational r)
constToConstant (LongDoubleConst s r _) = R.DoubleConst (fromRational r)
constToConstant (CharConst s c _)
  = error "constToConstant: No support for character constants."
constToConstant (StringConst ss s _)
  = error "constToConstant: No support for string constants."
constToConstant e = error ("constToConstant: Unhandled construct: " ++ show e)

declSpecToType :: TPEnv -> DeclSpec -> R.Type
declSpecToType env ds@(DeclSpec st tq ts _) = typSpecToType env ts
declSpecToType _ e = error ("declSpecToType: Unhandled construct: " ++ show e)

initToFunDecl :: TPEnv -> DeclSpec -> Init -> [Param] -> (TPEnv, Entity ())
initToFunDecl env ds is ps = (env', Proc iv ps' [] Nothing)
 where iv = fst $ initToName env (declSpecToType env' ds) is
       (env', ps') = mapAccumL paramToVariable env ps

initToNames :: TPEnv -> DeclSpec -> [Init] -> TPEnv
initToNames env ds is = updateEnv env vs
 where ivs = map (initToName env (declSpecToType env ds)) is
       vs = map (\(name, t) -> Variable t name) ivs

initToName :: TPEnv -> R.Type -> Init -> (String, R.Type)
initToName env tp (Init name dcl _ _ _ _) = (unId name, declToType tp dcl)

unOpToExp :: Expression () -> UnOp -> Expression ()
unOpToExp e AddrOf = R.AddrOf e
unOpToExp e Deref  = R.Deref e
unOpToExp (ConstExpr (R.IntConst n t)) Negate
  = ConstExpr (R.IntConst (-1*n) t)
unOpToExp (ConstExpr (R.FloatConst  n)) Negate
  = ConstExpr (R.FloatConst (-1*n))
unOpToExp (ConstExpr (R.DoubleConst n)) Negate
  = ConstExpr (R.DoubleConst (-1*n))
unOpToExp e Negate = FunctionCall (Function "-" (typeof e) Infix) [e]
unOpToExp e Positive = e
unOpToExp e Not = error "Not"
unOpToExp e Lnot = error "Lnot"

typToType :: TPEnv -> Type -> R.Type
typToType env (Type ds de _) = declSpecToType env ds
typToType _ t = error ("typToType: Unhandled construct: " ++ show t)

typSpecToType :: TPEnv -> TypeSpec -> R.Type
typSpecToType _ Tvoid{} = VoidType
typSpecToType _ Tchar{} = error "Tchar"
typSpecToType _ Tshort{} = error "TShort"
typSpecToType _ (Tint Nothing _) = NumType R.Unsigned S32
typSpecToType _ (Tint _ _) = NumType R.Signed S32
typSpecToType _ Tlong{} = error "Tlong"
typSpecToType _ Tlong_long{} = error "longlong"
typSpecToType _ Tfloat{} = FloatType
typSpecToType _ Tdouble{} = DoubleType
typSpecToType _ Tlong_double{} = error "long double"
-- Array types are incomplete so fake one. We recover the type elsewhere.
typSpecToType _ (Tstruct (Just (Id "array" _)) mfg attrs _)
  = ArrayType universal fakeType
-- Ivars are declared externally so just fake a type.
typSpecToType _ (Tstruct (Just (Id "ivar" _)) Nothing attrs _)
  = IVarType fakeType
-- Called for both declaration and use-sites.
typSpecToType env t'@(Tstruct (Just (Id s _)) mfg attrs _)
  | Just t <- findBuiltinDeclaration env s = t
  | Just t <- findLocalDeclaration env s = t
  | Nothing <- mfg = error ("typSpecToType: Internal error: " ++ show t'
                           ++ " not found in:\n" ++ show (headerDefs env)
                           ++ " and not in " ++ show (typedefs env))
  | otherwise = StructType s (map (fieldGroupToType env) (fromJust mfg))
typSpecToType _ Tunion{} = error "typSpecToType: No support for union."
typSpecToType _ Tenum{} = error "typSpecToType: No support for enum."
typSpecToType _ (Tnamed i@Id{} _ _) = namedToType (unId i)
typSpecToType _ TtypeofExp{} = error "typSpecToType: No support for typeofExp"
typSpecToType _ TtypeofType{} = error "typSpecToType: No support for typeofType"
typSpecToType _ Tva_list{} = error "typSpecToType: No support for valist."

namedToType :: String -> R.Type
namedToType "uint64_t" = NumType R.Unsigned S64
namedToType "uint40_t" = NumType R.Unsigned S40
namedToType "uint32_t" = NumType R.Unsigned S32
namedToType "uint16_t" = NumType R.Unsigned S16
namedToType "uint8_t"  = NumType R.Unsigned S8
namedToType "int64_t"  = NumType R.Signed S64
namedToType "int40_t"  = NumType R.Signed S40
namedToType "int32_t"  = NumType R.Signed S32
namedToType "int16_t"  = NumType R.Signed S16
namedToType "int8_t"   = NumType R.Signed S8
namedToType s          = error ("namedToType: Unrecognized type: " ++ s)

declToType :: R.Type -> Decl -> R.Type
declToType t DeclRoot{} = t
-- Truncate one level of pointers on array types.
declToType t (Ptr tqs DeclRoot{} _) | pointedArray t = t
declToType t (Ptr tqs dcl _)  = declToType (Pointer t) dcl
declToType t BlockPtr{} = error "Blocks?"
declToType t (Array tqs (NoArraySize _) dcl _) = NativeArray Nothing t
declToType t (Array tqs sz dcl _)
  = error ("declToType: No support for sized native arrays yet.")
declToType t (Proto dcl params _)
  = trace ("DEBUG: Proto: " ++ show params) $ t
declToType t (OldProto dcl ids _) = error "OldProto"
declToType _ d = error ("declToType: Unhandled construct: " ++ show d)

pointedArray :: R.Type -> Bool
pointedArray (ArrayType{}) = True
pointedArray (Pointer t)   = pointedArray t
pointedArray _             = False

fieldGroupToType :: TPEnv -> FieldGroup -> (String, R.Type)
fieldGroupToType env (FieldGroup dss [fs] _)
  = (fieldToName fs, declSpecToType env dss)
fieldGroupToType _ g
  = error ("fieldGroupToType: Unhandled construct: " ++ show g)

signToSign :: Signed -> Signedness
signToSign Signed   = R.Signed
signToSign Unsigned = R.Unsigned

fieldToName :: Field -> String
fieldToName (Field (Just s) _ _ _) = unId s

unId :: Id -> String
unId (Id n _) = n
unId i = error ("unId: Unhandled construct: " ++ show i)

-- Some place holders.
fakeVar :: R.Variable ()
fakeVar = Variable fakeType "FAKE"
fakeType :: R.Type
fakeType = VoidType

-- Feldspar "builtins".
builtins :: [(String, R.Variable ())]
builtins =
  [ ("getLength", Variable (NumType R.Unsigned S32) "getLength")
  ]

findBuiltinDeclaration :: TPEnv -> String -> Maybe R.Type
findBuiltinDeclaration env s = go (headerDefs env) s
  where go [] s = Nothing
        go ((_, _, StructDef n fs):t) s
         | n == s = Just (StructType n (map toType fs))
         | otherwise = go t s
        go e s = error ("findBuiltinDeclaration: Non-Struct found: " ++ show e)
        toType (StructMember n t) = (n, t)

findLocalDeclaration :: TPEnv -> String -> Maybe R.Type
findLocalDeclaration env s = go (typedefs env) s
  where go [] s = Nothing
        go (tp@(StructType n _):t) s | n == s = Just tp
                                     | otherwise = go t s
        go e s = error ("findLocalDeclaration: Non-Struct found: " ++ show e)

-- Environments.
data TPEnv = TPEnv
    { vars :: [(String, Variable ())]
    , typedefs :: [R.Type]
    , headerDefs :: [(String, [R.Type], Entity ())]
    } deriving Show

emptyEnv :: [(String, [R.Type], Entity ())] -> TPEnv
emptyEnv hDefs = TPEnv [] [] hDefs

updateEnv :: TPEnv -> [R.Variable ()] -> TPEnv
updateEnv env ns = env { vars = nt ++ vars env }
     where nt = map (\v@(Variable t name) -> (name, v)) ns

updateEnv2 :: TPEnv -> [R.Variable ()] -> R.Type -> TPEnv
updateEnv2 (TPEnv vs tdefs hdefs) ns t
 = TPEnv (nt ++ vs) (t:tdefs) hdefs
     where nt = map (\v@(Variable t name) -> (name, v)) ns

-- Add two environments.
plusEnv :: TPEnv -> TPEnv -> TPEnv
plusEnv (TPEnv vs1 tdefs1 hdefs) (TPEnv vs2 tdefs2 _ )
  = TPEnv (vs1 ++ vs2) (tdefs1 ++ tdefs2) hdefs

-- Patch the type information in the environment when we learn more.
fixupEnv :: TPEnv -> String -> R.Type -> TPEnv
fixupEnv env s tp = env { vars = go (vars env) }
  where go [] = []
        go (p@(n,(Variable (ArrayType r _)  n')):t)
         | s == n = (n, (Variable (ArrayType r tp) n')):go t
        go (p:t) = p:go t

-- Misc helpers

patchHdefs :: [Entity ()] -> [(String, [R.Type], Entity ())]
patchHdefs [] = []
patchHdefs (d@(StructDef s members):t)
  = (s, map snd ts, StructDef s $ map toDef ts):patchHdefs t
  where [(StructType _ ts)] = decodeType s
        toDef (n,t') = StructMember n t'
patchHdefs (p@(Proc n ins outs Nothing):t)
  = (n, map typeof (ins++outs), p):patchHdefs t
patchHdefs (e:t) = patchHdefs t

mkDef :: R.Type -> [Entity ()]
mkDef (StructType n fields)
  = [StructDef n (map (\(n,t) -> StructMember n t) fields)]
-- Only interested in struct definitions so discard everything else.
mkDef _ = []

lookup3 :: String -> [(String, [R.Type], Entity ())] -> R.Type
lookup3 s xs = head ts
  where (_, ts, _) = head $ filter (\(s1,_,_) -> s1 == s) xs

-- Input helpers.

-- The C parser chokes when parsing a function call that has a type
-- parameter as first argument. This is precisely what we have with our
-- "at" macro. We can reconstruct the first argument by other means, so
-- just change all calls on the form "at(type,p1,p2)" to "at(p1, p2)".
massageInput:: B.ByteString -> B.ByteString
massageInput xs = foldr (\w xs -> dropBitMask xs w) tmp otherWords'
 where -- Drop first parameter for these functions.
       prefixWords = map B.pack ["at(", "ivar_put(","ivar_get_nontask("]
       -- Drop some parameters according to mask for these.
       otherWords = [ ("spawn2(", [True,False,True,False, True])
                    , ("run2(", [True, False, False])]
       otherWords' = map (\(p1, b) -> (B.pack p1, b)) otherWords
       tmp = foldr (\w xs -> dropFirstArg xs w) xs prefixWords

dropFirstArg:: B.ByteString -> B.ByteString -> B.ByteString
dropFirstArg xs wrd = go (B.breakSubstring wrd xs) []
  where go (h, t) acc
          | B.null t = B.append (B.concat $ reverse acc) h
          | otherwise = go (B.breakSubstring wrd $ fixArg t) (wrd:h:acc)
        -- Drops the prefix "<word>.*,".
        fixArg = B.drop 1 . B.dropWhile (/= ',') . B.drop (B.length wrd)

dropBitMask :: B.ByteString -> (B.ByteString, [Bool]) -> B.ByteString
dropBitMask xs (wrd, bs) = go (B.breakSubstring wrd xs) []
  where comma = B.pack ","
        rparen = B.pack ")"
        go (h, t) acc
          | B.null t = B.append (B.concat $ reverse acc) h
          | otherwise = go (B.breakSubstring wrd rest) (rparen:as:wrd:h:acc)
             where (as, rest) = fixArg (B.span (/= ';') $ B.drop (B.length wrd) t)
        -- Drops the types.
        fixArg (as, rest) = (flt $ zip bs (B.split ',' $ B.init as), rest)
          where flt = B.concat  . intersperse comma . map snd . filter fst
