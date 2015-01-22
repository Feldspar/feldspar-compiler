module Feldspar.Compiler.Imperative.ExternalProgram (parseFile, massageInput) where

import Feldspar.Lattice (universal)
import Feldspar.Compiler.Imperative.FromCore.Interpretation (decodeType)
import qualified Feldspar.Compiler.Imperative.Representation as R
import Feldspar.Compiler.Imperative.Representation hiding (
  Block, Switch, Assign, Cast, IntConst, FloatConst, DoubleConst, Type,
  Deref, AddrOf, Unsigned, Signed, inParams)
import Feldspar.Compiler.Imperative.Frontend (litB, toBlock, fun, fun', call)

import qualified Data.ByteString.Char8 as B
import qualified Language.C.Parser as P
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
parseFile filename s hDefs =
  case P.parse [C99] builtin_types P.parseUnit s' (startPos filename) of
      Left err -> Nothing
      Right defs -> Just (Module $ fhDefs ++ toProgram hDefs defs)
   where s' = massageInput s
         fhDefs = filter isStructDef hDefs
         isStructDef StructDef{} = True
         isStructDef _           = False

-- A list of built in types. We need the regular C99 types in this list, or
-- the parser will die with a mysterious parse error.
builtin_types :: [String]
builtin_types = [ "uint64_t", "uint32_t", "uint16_t", "uint8_t"
                , "int64_t", "int32_t", "int16_t", "int8_t"
                , "bool"]

toProgram :: [Entity ()] -> [Definition] -> [Entity ()]
toProgram [] defs = snd $ defsToProgram (emptyEnv []) defs
toProgram hDefs defs = rest' ++ reverse funcs'
  where funcs' = snd $ defsToProgram env' $ reverse funcs
        (env', rest') = defsToProgram (emptyEnv (patchHdefs hDefs)) rest
        (funcs, rest) = span isFunc defs
        isFunc FuncDef{} = True
        isFunc _         = False

defsToProgram :: TPEnv -> [Definition] -> (TPEnv, [Entity ()])
defsToProgram = mapAccumL defToProgram

defToProgram :: TPEnv -> Definition -> (TPEnv, Entity ())
defToProgram env (FuncDef func _) = funcToProgram env func
defToProgram env (DecDef (InitGroup ds attr is@[Init n Array{} Nothing (Just (CompoundInitializer ins _)) _ _] _) _)
  = valueToProgram env ds is n ins'
   where ins' = map snd ins
defToProgram env (DecDef ig _) = initGroupToDeclaration env ig
defToProgram _ (EscDef s _) = error ("defToProgram: " ++ show s)
defToProgram _ e = error ("defToProgram: Unhandled construct: " ++ show e)

funcToProgram :: TPEnv -> Func -> (TPEnv, Entity ())
funcToProgram env (Func ds name decl (Params parms _ _) bis _)
  = (env'', Proc (unId name) False inParams outParams (Just bs))
   where (env', vs) = mapAccumL paramToVariable env parms
         (env'', bs) = blockToBlock env' bis
         (inParams, outParams)
           | VoidType <- dsl = (init vs, Left [last vs])
           | otherwise       = (vs, Right $ Variable dsl "out")
         dsl = declSpecToType env ds
funcToProgram _ e = error ("funcToProgram: Unhandled construct: " ++ show e)

valueToProgram :: TPEnv -> DeclSpec -> [Init] -> Id -> [Initializer]
               -> (TPEnv, Entity ())
valueToProgram env ds is n ins = (env', ValueDef (nameToVariable env' n) cs')
  where env' = initToNames env ds is
        cs = map (\(ExpInitializer (Const c _) _) -> constToConstant c) ins
        cs' = ArrayConst (map (castConstant t) cs) t
        t = declSpecToType env ds

paramToVariable :: TPEnv -> Param -> (TPEnv, R.Variable ())
paramToVariable env (Param (Just id) t p _)
 | Just v <- lookup (unId id) (vars env) = (env, v) -- We have recovered types.
 | otherwise = (updateEnv env [v], v)
  where v = Variable (declToType (declSpecToType env t) p) (unId id)
paramToVariable _ e = error ("paramToVariable: Unhandled construct: " ++ show e)

blockToBlock :: TPEnv -> [BlockItem] -> (TPEnv, R.Block ())
blockToBlock env bis = (env'', R.Block (concat ds) (Sequence bs))
  where (env', ds) = mapAccumL blockDeclToDecl env decls
        (env'', bs) = blockItemsToProgram env' rest
        (decls, rest) = span isBlockDecl bis
        isBlockDecl BlockDecl{} = True
        isBlockDecl _           = False

blockDeclToDecl :: TPEnv -> BlockItem -> (TPEnv, [Declaration ()])
blockDeclToDecl env (BlockDecl ig) = (env', dv)
  where env' = initGroupToProgram env ig
        igv = take (length (vars env') - length (vars env)) (vars env')
        dv = map (\(_, v) -> Declaration v Nothing) igv -- Program decl

blockItemsToProgram :: TPEnv -> [BlockItem] -> (TPEnv, [Program ()])
blockItemsToProgram = mapAccumL blockItemToProgram

blockItemToProgram :: TPEnv -> BlockItem -> (TPEnv, Program ())
blockItemToProgram _ b@BlockDecl{}
  = error ("Declaration in the middle of a block: " ++ show b)
-- Ivar reconstruction stuff.
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "ivar_get" _) _) _ _)) _))
  = (env, ProcedureCall "ivar_get" (tp:map ValueParameter es))
    where (FunctionCall (Function "ivar_get" _) es) = expToExpression env e
          tp = TypeParameter $ typeof (R.Deref (head es))
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "ivar_get_nontask" _) _) _ _)) _))
  = (env, ProcedureCall "ivar_get_nontask" (tp:map ValueParameter es))
    where (FunctionCall (Function "ivar_get_nontask" _) es) = expToExpression env e
          tp = TypeParameter $ typeof (R.Deref (head es))
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "ivar_put" _) _) _ _)) _))
  = (env, ProcedureCall s (tp:map ValueParameter es))
   where (FunctionCall (Function s _) es) = expToExpression env e
         tp = TypeParameter (typeof (R.Deref (last es)))
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "run2" _) _) _ _)) _))
  = (env, ProcedureCall s (FunParameter e1:tp))
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1)]) = expToExpression env e
         tp = map TypeParameter $ lookup3 e1 (headerDefs env)
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "run3" _) _) _ _)) _))
  = (env, ProcedureCall s (FunParameter e1:tp))
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1)]) = expToExpression env e
         tp = map TypeParameter $ lookup3 e1 (headerDefs env)
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "run4" _) _) _ _)) _))
  = (env, ProcedureCall s (FunParameter e1:tp))
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1)]) = expToExpression env e
         tp = map TypeParameter $ lookup3 e1 (headerDefs env)
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "run5" _) _) _ _)) _))
  = (env, ProcedureCall s (FunParameter e1:tp))
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1)]) = expToExpression env e
         tp = map TypeParameter $ lookup3 e1 (headerDefs env)
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "spawn2" _) _) _ _)) _))
  = (env, ProcedureCall s es)
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1),e2,e3]) = expToExpression env e
         es = [ FunParameter e1, TypeParameter (typeof e2), ValueParameter e2
              , TypeParameter (typeof e3), ValueParameter e3]
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "spawn3" _) _) _ _)) _))
  = (env, ProcedureCall s es)
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1),e2,e3,e4]) = expToExpression env e
         es = [ FunParameter e1, TypeParameter (typeof e2), ValueParameter e2
              , TypeParameter (typeof e3), ValueParameter e3
              , TypeParameter (typeof e4), ValueParameter e4]
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "spawn4" _) _) _ _)) _))
  = (env, ProcedureCall s es)
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1), e2, e3, e4, e5]) = expToExpression env e
         es = [ FunParameter e1, TypeParameter (typeof e2), ValueParameter e2
              , TypeParameter (typeof e3), ValueParameter e3
              , TypeParameter (typeof e4), ValueParameter e4
              , TypeParameter (typeof e5), ValueParameter e5]
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "spawn5" _) _) _ _)) _))
  = (env, ProcedureCall s es)
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1), e2, e3, e4, e5, e6]) = expToExpression env e
         es = [ FunParameter e1, TypeParameter (typeof e2), ValueParameter e2
              , TypeParameter (typeof e3), ValueParameter e3
              , TypeParameter (typeof e4), ValueParameter e4
              , TypeParameter (typeof e5), ValueParameter e5
              , TypeParameter (typeof e6), ValueParameter e6]
-- Probably copyArray.
blockItemToProgram env (BlockStm (Exp (Just e@FnCall{}) _))
  = (env, ProcedureCall s (map ValueParameter es))
   where (FunctionCall (Function s _) es) = expToExpression env e
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
stmToProgram env (Block bis _) = Sequence (snd $ blockItemsToProgram env bis)
stmToProgram env (If e b1@(Block alt _) (Just b2@(Block alt' _)) _)
  = R.Switch cond [ (Pat $ litB True, toBlock $ stmToProgram env b1)
                  , (Pat $ litB False, toBlock $ stmToProgram env b2)]
    where cond = expToExpression env e
stmToProgram env (Switch e (Block alts _) _)
  = R.Switch (expToExpression env e) $ concatMap (switchAltToProgram env) alts
stmToProgram env (While e s _) = SeqLoop cond (toBlock Empty) (toBlock p)
  where cond = expToExpression env e
        p = stmToProgram env s
stmToProgram _ (DoWhile s e _) = error "stmToProgram: No support for Do."
stmToProgram env (For (Left es@(InitGroup ds attr [Init n _ Nothing (Just (ExpInitializer e0 _)) _ _] _))
                      (Just (BinOp Lt name@Var{} v2 _))
                      (Just (Assign lhs AddAssign rhs _)) s _)
  = ParLoop Sequential v' (expToExpression env' e0) (expToExpression env' v2) (expToExpression env' rhs) body
    where env' = initGroupToProgram env es
          v' = varToVariable env' name
          body = toBlock $ stmToProgram env' s
stmToProgram _ Goto{} = error "stmToProgram: No support for goto."
stmToProgram _ Continue{} = error "stmToProgram: No support for continue."
stmToProgram _ Break{} = error "stmToProgram: Unexpected break."
stmToProgram env (Return (Just e) _)
  = call "return" [ValueParameter $ expToExpression env e]
stmToProgram _ (Pragma _ _) = error "Pragma not supported yet."
stmToProgram _ a@Asm{} = error ("stmToProgram: unexpected asm: " ++ show a)
stmToProgram _ e = error ("stmToProgram: Unhandled construct: " ++ show e)

impureExpToProgram :: TPEnv -> Exp -> (TPEnv, Program ())
-- Hook for padding incomplete struct array * type info.
impureExpToProgram env (Assign e JustAssign
                               f@(FnCall (Var (Id fn _) _)
                                         [e1', e2', e3'] _) _)
  | fn == "initArray" || fn == "setLength"
  = (env', R.Assign (Just $ expToExpression env' e) (expToExpression env' f))
   where env' = fixupEnv env e $ typToType env (getTyp e2')
         getTyp (SizeofType e _) = e
         getTyp (BinOp Sub _ e _) = getTyp e
         getTyp e = error ("Unexpected parameter to initArray/setLength:" ++ show e)
impureExpToProgram env (Assign e@Var{} JustAssign
                               f@(FnCall (Var (Id "at" _) _) [e1, e2] _) _)
  = (env', R.Assign (Just $ expToExpression env e) (expToExpression env' f))
   where env' = fixupEnv env e1 $ varType (varToVariable env e)
impureExpToProgram env (Assign e@(FnCall (Var (Id "at" _) _) [e1, e2] _) JustAssign
                               f@(FnCall (Var (Id "at" _) _) [e1', e2'] _) _)
  = (env'', R.Assign (Just $ expToExpression env'' e) (expToExpression env'' f))
   -- Hope LHS or RHS has proper types. Propagate to the other side.
   where env'' = fixupEnv env' e1 $ typeof (expToExpression env' f)
         env'  = fixupEnv env e1' $ typeof (expToExpression env e)
impureExpToProgram env (Assign e1 JustAssign e2 _)
  = (env, R.Assign (Just $ expToExpression env e1) (expToExpression env e2))
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
expToExpression env (BinOp op e1 e2 _) = opToFunctionCall parms op
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
expToExpression _ (Cond c e1 e2 _) = error "expToExpression: No support for conditional statements."
expToExpression env (Member e name _)
  = StructField (expToExpression env e) (unId name)
expToExpression _ PtrMember{} = error "expToExpression: No support for ptrmember."
expToExpression env (Index e1 e2 _)
  = ArrayElem (expToExpression env e1) (expToExpression env e2)
expToExpression env (FnCall (Var (Id "at" _) _) [e1, e2] _)
  = ArrayElem (expToExpression env e1) (expToExpression env e2)
expToExpression env (FnCall e es _)
  = expToFunctionCall env (map (expToExpression env) es) e
expToExpression _ CudaCall{} = error "expToExpression: No support for CUDA."
expToExpression _ Seq{} = error "expToExpression: No support for seq."
expToExpression env (CompoundLit t ls _) = ConstExpr $ ArrayConst cs' $ typToType env t
  where cs = map (\(_, ExpInitializer (Const c _) _) -> constToConstant c) ls
        cs' = map (castConstant (typToType env t)) cs
expToExpression _ StmExpr{} = error "expToExpression: No support for Stmexpr."
expToExpression _ BuiltinVaArg{} = error "expToExpression: varargs not supported."
expToExpression _ BlockLit{} = error "expToExpression: No support for blocklit."
expToExpression _ e = error ("expToExpression: Unhandled construct: " ++ show e)

opToFunctionCall :: [Expression ()] -> BinOp -> Expression ()
opToFunctionCall es op = case opToString op of
                      Right s -> fun' t s es
                      Left s -> fun' (MachineVector 1 BoolType) s es
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

expToFunctionCall :: TPEnv -> [Expression ()] -> Exp -> Expression ()
expToFunctionCall _ es (Var name _)
  | Just v <- lookup (unId name) builtins
  = fun (varType v) (varName v) es
  | otherwise = fun fakeType (unId name) es

-- Signed integers are the default for literals, but that is not always
-- convenient. Fix things up afterwards instead.
castConstant :: R.Type -> R.Constant () -> R.Constant ()
castConstant (MachineVector 1 t) (R.IntConst i _) = R.IntConst i t
castConstant _ c = error ("castConstant: Unexpected argument: " ++ show c)

constToConstant :: Const -> Constant ()
constToConstant (IntConst _ sgn i _)
  = R.IntConst i (NumType (signToSign sgn) S32)
constToConstant (LongIntConst _ sgn i _)
  = R.IntConst i (NumType (signToSign sgn) S64)
constToConstant (LongLongIntConst _ sgn i _)
  = R.IntConst i (NumType (signToSign sgn) S64)
constToConstant (FloatConst _ r _) = R.FloatConst (fromRational r)
constToConstant (DoubleConst _ r _) = R.DoubleConst (fromRational r)
constToConstant (LongDoubleConst _ r _) = R.DoubleConst (fromRational r)
constToConstant (CharConst _ c _)
  = error "constToConstant: No support for character constants."
constToConstant (StringConst ss s _)
  = error "constToConstant: No support for string constants."
constToConstant e = error ("constToConstant: Unhandled construct: " ++ show e)

declSpecToType :: TPEnv -> DeclSpec -> R.Type
declSpecToType env ds@(DeclSpec st tq ts _) = typSpecToType env ts
declSpecToType _ e = error ("declSpecToType: Unhandled construct: " ++ show e)

initToFunDecl :: TPEnv -> DeclSpec -> Init -> [Param] -> (TPEnv, Entity ())
initToFunDecl env ds is ps = (env', Proc iv False ps' (Left []) Nothing)
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
unOpToExp e Negate = fun' (typeof e) "-" [e]
unOpToExp e Positive = e
unOpToExp e Not = error "Not"
unOpToExp e Lnot = fun' (MachineVector 1 BoolType) "!" [e]

typToType :: TPEnv -> Type -> R.Type
typToType env (Type ds de _) = declSpecToType env ds
typToType _ t = error ("typToType: Unhandled construct: " ++ show t)

typSpecToType :: TPEnv -> TypeSpec -> R.Type
typSpecToType _ Tvoid{} = VoidType
typSpecToType _ Tchar{} = error "Tchar"
typSpecToType _ Tshort{} = error "TShort"
typSpecToType _ (Tint Nothing _) = MachineVector 1 (NumType R.Unsigned S32)
typSpecToType _ (Tint _ _) = MachineVector 1 (NumType R.Signed S32)
typSpecToType _ Tlong{} = error "Tlong"
typSpecToType _ Tlong_long{} = error "longlong"
typSpecToType _ Tfloat{} = MachineVector 1 FloatType
typSpecToType _ Tdouble{} = MachineVector 1 DoubleType
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
namedToType "uint64_t" = MachineVector 1 (NumType R.Unsigned S64)
namedToType "uint40_t" = MachineVector 1 (NumType R.Unsigned S40)
namedToType "uint32_t" = MachineVector 1 (NumType R.Unsigned S32)
namedToType "uint16_t" = MachineVector 1 (NumType R.Unsigned S16)
namedToType "uint8_t"  = MachineVector 1 (NumType R.Unsigned S8)
namedToType "int64_t"  = MachineVector 1 (NumType R.Signed S64)
namedToType "int40_t"  = MachineVector 1 (NumType R.Signed S40)
namedToType "int32_t"  = MachineVector 1 (NumType R.Signed S32)
namedToType "int16_t"  = MachineVector 1 (NumType R.Signed S16)
namedToType "int8_t"   = MachineVector 1 (NumType R.Signed S8)
namedToType "bool"     = MachineVector 1 BoolType
namedToType s          = error ("namedToType: Unrecognized type: " ++ s)

declToType :: R.Type -> Decl -> R.Type
declToType t DeclRoot{} = t
-- Truncate one level of pointers on array types.
declToType t (Ptr tqs DeclRoot{} _) | pointedArray t = t
declToType t (Ptr tqs dcl _)  = declToType (MachineVector 1 (Pointer t)) dcl
declToType t BlockPtr{} = error "Blocks?"
declToType t (Array tqs (NoArraySize _) dcl _) = NativeArray Nothing t
declToType t (Array tqs sz dcl _)
  = error "declToType: No support for sized native arrays yet."
declToType t (Proto dcl params _)
  = trace ("DEBUG: Proto: " ++ show params) t
declToType t (OldProto dcl ids _) = error "OldProto"
declToType _ d = error ("declToType: Unhandled construct: " ++ show d)

pointedArray :: R.Type -> Bool
pointedArray (ArrayType{})                 = True
pointedArray (MachineVector _ (Pointer t)) = pointedArray t
pointedArray _                             = False

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
fakeType :: R.Type
fakeType = VoidType

-- Feldspar "builtins".
builtins :: [(String, R.Variable ())]
builtins =
  [ ("getLength", Variable (MachineVector 1 (NumType R.Unsigned S32)) "getLength")
  ]

findBuiltinDeclaration :: TPEnv -> String -> Maybe R.Type
findBuiltinDeclaration env = go (headerDefs env)
  where go [] s = Nothing
        go ((_, _, StructDef n fs):t) s
         | n == s = Just (StructType n (map toType fs))
         | otherwise = go t s
        go e s = error ("findBuiltinDeclaration: Non-Struct found: " ++ show e)
        toType (StructMember n t) = (n, t)

findLocalDeclaration :: TPEnv -> String -> Maybe R.Type
findLocalDeclaration env = go (typedefs env)
  where go [] s = Nothing
        go (tp@(StructType n _):t) s | n == s = Just tp
                                     | otherwise = go t s
        go e s = error ("findLocalDeclaration: Non-Struct found: " ++ show e)

-- | Environments.
data TPEnv = TPEnv
    { vars :: [(String, Variable ())]
    , typedefs :: [R.Type]
    , headerDefs :: [(String, [R.Type], Entity ())]
    } deriving Show

emptyEnv :: [(String, [R.Type], Entity ())] -> TPEnv
emptyEnv = TPEnv [] []

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
fixupEnv :: TPEnv -> Exp -> R.Type -> TPEnv
fixupEnv env _ VoidType = env -- No new type information.
fixupEnv env (UnOp Deref (Var (Id s _) _) _) tp = env { vars = goVar (vars env) }
  where goVar [] = []
        goVar (p@(n, Variable (MachineVector l (Pointer (ArrayType r _))) n'):t)
         | s == n = (n, Variable (MachineVector l (Pointer (ArrayType r tp))) n'):goVar t
        goVar (p:t) = p:goVar t
fixupEnv _ (UnOp Deref e _) _ = error ("fixupEnv: No support for " ++ show e)
fixupEnv env (Var (Id s _) _) tp = env { vars = goVar (vars env) }
  where goVar [] = []
        goVar (p@(n, Variable (ArrayType r _) n'):t)
         | s == n = (n, Variable (ArrayType r tp) n'):goVar t
        goVar (p:t) = p:goVar t
fixupEnv env (Member (Var (Id s _) _) (Id name _) _) tp = env { vars = goStruct (vars env) }
  where goStruct [] = []
        goStruct (p@(n, Variable (StructType s' ns) n'):t)
         | s == n
         , Just v <- lookup name ns
         = (n, Variable (StructType s' ns') n'):goStruct t
           where ns' = map structFixup ns
                 structFixup (mem, ArrayType r _)
                  | mem == name = (mem, ArrayType r tp)
                 structFixup (mem, othertype)
                  | mem == name = (mem, tp)
                 structFixup e = e
        goStruct (p:t) = p:goStruct t
fixupEnv env (Member (FnCall (Var (Id "at" _) _) [Var (Id s _) _, _] _) (Id name _) _) tp = env { vars = goStructAt (vars env) }
  where goStructAt [] = []
        goStructAt (p@(n, Variable (StructType s' ns) n'):t)
         | s == n
         , Just v <- lookup name ns
         = (n, Variable (StructType s' ns') n'):goStructAt t
           where ns' = map structFixup ns
                 structFixup e@(mem, ArrayType r _)
                  | mem == name = (mem, ArrayType r tp)
                 structFixup e = e
        goStructAt (p:t) = p:goStructAt t
fixupEnv env e tp = env

-- Misc helpers

patchHdefs :: [Entity ()] -> [(String, [R.Type], Entity ())]
patchHdefs [] = []
patchHdefs (StructDef s members:t)
  = (s, map snd ts, StructDef s $ map toDef ts):patchHdefs t
  where [StructType _ ts] = decodeType s
        toDef (n,t') = StructMember n t'
patchHdefs (p@(Proc n _ ins outs Nothing):t)
  = (n, map typeof ins ++ etypeof outs, p):patchHdefs t
   where etypeof (Left es) = map typeof es
         etypeof (Right e) = [typeof e]
patchHdefs (_:t) = patchHdefs t

mkDef :: R.Type -> [Entity ()]
mkDef (StructType n fields)
  = [StructDef n (map (\(n,t) -> StructMember n t) fields)]
-- Only interested in struct definitions so discard everything else.
mkDef _ = []

lookup3 :: String -> [(String, [R.Type], Entity ())] -> [R.Type]
lookup3 s xs = ts
  where (_, ts, _) = head $ filter (\(s1,_,_) -> s1 == s) xs

-- Input helpers.

-- The C parser chokes when parsing a function call that has a type
-- parameter as first argument. This is precisely what we have with our
-- "at" macro. We can reconstruct the first argument by other means, so
-- just change all calls on the form "at(type,p1,p2)" to "at(p1, p2)".
massageInput:: B.ByteString -> B.ByteString
massageInput xs = foldr (\w xs -> dropBitMask xs w) tmp otherWords'
 where -- Drop first parameter for these functions.
       prefixWords = map B.pack ["at(", "ivar_put(","ivar_get(", "ivar_get_nontask("]
       -- Drop some parameters according to mask for these.
       otherWords = [ ("spawn2(", [True, False, True, False, True])
                    , ("run2(", [True, False, False])
                    , ("spawn3(", [True, False, True, False, True
                                       , False, True])
                    , ("run3(", [True, False, False, False])
                    , ("spawn4(", [True, False, True, False, True
                                       , False, True, False, True])
                    , ("run4(", [True, False, False, False, False])
                    , ("spawn5(", [True, False, True, False, True
                                       , False, True, False, True
                                       , False, True])
                    , ("run5(", [True, False, False, False, False,False])]
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
