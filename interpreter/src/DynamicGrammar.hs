{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -fdefer-typed-holes #-}

module DynamicGrammar where


import Prelude hiding (exp, error, unlines)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import qualified AbsGrammar as Abs
import ParGrammar
import LexGrammar
import Control.Monad.Reader

import ErrM

type Program = [Abs.TopDef]

type ParseFun a = [Token] -> Err a

type Var = String
type Def = (Var, Exp)

-- Possible values of binds in map (may be evaluated or not)
data Binds
  = Strict DynVal
  | Lazy Exp
  deriving (Show)
  
data Environment = Environment
  { bindings :: Map.Map Var Exp
  , resolve_values :: Bool
  } deriving (Show)

data DynVal
  = TInt Integer
  | TBool Bool
--  | TTuple [DynVal]
--  identyfikatory typów polimorficznych (listy i booleany będą tutaj)
--deriving (Show, Eq)

data Exp
  = EApp Exp Exp
  | EIf Exp Exp Exp
  | ELet [Def] Exp
  | ELam Exp Var
  | EOp Op Exp Exp
  | EVar Var
  | EVal DynVal
  deriving (Show)

data RuntimeError
  = AppTypeMismatch FuncId [Exp]
  | UnboundVar Var Environment
  | WrongType Exp Exp
  deriving (Show)

data CompilationError
  = NoMainDefinition
  | MultipleDefinitions Var
  | UnsupportedSyntaxExpr Abs.Exp
  | UnsupportedSyntaxDef  Abs.VDef
  deriving (Show)

data FuncId
  = FuncVar Var
  | FuncOp  Op
  deriving (Show)

data Op
  = OpAdd
  | OpMul
  | OpSub
  | OpAnd
  | OpOr
  | OpLes
  | OpEqu
  deriving (Show)

return_ :: (Show a, Monad m) => a -> m a
return_ a = trace (show a) (Prelude.return a)

indent :: Exp -> String
indent exp = runReader (print_indent exp) 0

make_indent :: Int -> String
make_indent n = take n $ repeat ' '

unlines :: [String] -> String
unlines [] = []
unlines [line] = line
unlines (l:ls) = l ++ ('\n' : (unlines ls))

print_indent :: Exp -> Reader Int String
print_indent (EApp fun arg) = do
  funS <- print_indent fun
  argS <- local (+2) (print_indent arg)
  return $ unlines [funS,argS]

print_indent (EIf cond true false) = do
  condS <- print_indent cond
  trueS  <- local (+2) (print_indent true)
  falseS <- local (+2) (print_indent false)
  return $ unlines [condS, trueS, falseS]

print_indent (ELet defs finalExp) = do
  n <- ask
  let letS = (make_indent n)++"let"
  defsS <- mapM print_def defs
  let inS = (make_indent n)++"in"
  expS <- local (+2) $ print_indent finalExp
  return $ unlines $ [letS] ++ defsS ++ [inS, expS]
    where
      print_def :: Def -> Reader Int String
      print_def (def, exp) = do
        n <- ask
        let defS = (make_indent (n+2)) ++ def ++ " ="
        expS <- local (+4) (print_indent exp)
        return $ unlines [defS, expS]

print_indent (ELam exp var) = do
  n <- ask
  let varS = (make_indent n) ++ var
  expS <- local (+2) (print_indent exp)
  return $ unlines $ [varS, expS]

print_indent (EVar var) = do
  n <- ask
  return $ unlines [(make_indent n) ++ var]

print_indent (EOp op exp1 exp2) = do
  n <- ask
  let opS = (make_indent n) ++ show op
  exp1S <- local (+2) (print_indent exp1)
  exp2S <- local (+2) (print_indent exp2)
  return $ unlines [opS, exp1S, exp2S]

print_indent (EVal value) = do
  n <- ask
  return $ (make_indent n) ++ show value

parseGen :: ParseFun a -> String -> Err a
parseGen p s = p $ myLexer s

parse :: String -> Err Program
parse = parseGen pListTopDef

desugar_prog :: Program -> Err Exp
desugar_prog program = do
    let vdefs = program >>= extract_vdef
    defs <- desugar_vdefs vdefs
    let 
    case partition (\(name, _) -> name == "main") defs of
      ([(_, main)], rest) -> return $ ELet rest main
      ([], _)             -> Bad $ show $ NoMainDefinition
      _                   -> Bad $ show $ MultipleDefinitions "main"   
    where
      extract_vdef (Abs.VarDef vdef) = [vdef]
      extract_vdef _                 = []

desugar_vdefs :: [Abs.VDef] -> Err [Def]
desugar_vdefs vdefs = sequence $ map desugar_vdef vdefs
    
desugar_vdef :: Abs.VDef -> Err Def
desugar_vdef def@(Abs.Def (Abs.Ident ident) binds exp) = lambda
  where
    lambda = do
      bound <- foldr add_bind (desugar exp) binds
      return $ (ident, bound)
    add_bind :: Abs.Bind -> Err Exp -> Err Exp
    add_bind (Abs.BVar (Abs.Ident bind_ident)) (Ok result) = Ok $ ELam result bind_ident
    add_bind _                                 err@(Bad _) = err
    add_bind _                                 _           = Bad $ show $ UnsupportedSyntaxDef def

desugar :: (Abs.Exp) -> Err Exp
-- desugar (Abs.EMat _)
desugar (Abs.ELet vdefs exp) = do
  defs <- desugar_vdefs vdefs
  dexp <- desugar exp
  return $ ELet defs dexp
desugar (Abs.EIf cond true false) = (liftM3 EIf) (desugar cond) (desugar true) (desugar false)
desugar (Abs.ESmal left right) = (liftM2 $ EOp OpLes) (desugar left) (desugar right) 
desugar (Abs.EEq left right) = (liftM2 $ EOp OpEqu) (desugar left) (desugar right)
desugar (Abs.EAnd left right) = (liftM2 $ EOp OpAnd) (desugar left) (desugar right)
desugar (Abs.EOr left right) = (liftM2 $ EOp OpOr) (desugar left) (desugar right)

desugar (Abs.EAdd left right) = (liftM2 $ EOp OpAdd) (desugar left) (desugar right)
desugar (Abs.ESub left right) = (liftM2 $ EOp OpSub) (desugar left) (desugar right)
desugar (Abs.EMul left right) = (liftM2 $ EOp OpMul) (desugar left) (desugar right)
-- desugar (Abs.Cons _ _)
desugar (Abs.EApp func value) = (liftM2 EApp) (desugar func) (desugar value)
-- desugar (ELis _)
-- desugar (ETup _)
desugar (Abs.EInt int) = Ok $ EVal $ TInt int
desugar (Abs.ECon (Abs.UIdent ident)) = Ok $ EVar ident
desugar (Abs.EVar (Abs.Ident ident)) = Ok $ EVar ident
desugar (Abs.ETup [exp]) = desugar exp

desugar unsupported = Bad $ show $ UnsupportedSyntaxExpr unsupported



interpret :: Exp -> Err Exp
interpret exp = runReader (helpEvalExp exp) env
  where
    baseMap = Map.fromList [("True", EVal $ TBool True),
                            ("False", EVal $ TBool False)
                           ]
    env = Environment baseMap True
                      

addBinding :: Var -> Exp -> Environment -> Environment
addBinding var exp env = env {bindings = Map.insert var exp (bindings env) }

turn_off_resolving :: Environment -> Environment
turn_off_resolving env = env { resolve_values = False }

{-
trans :: (a -> Reader r b) -> Reader r (a -> b)
trans f = do
    r <- ask
    Prelude.return $ \a -> runReader (f a) r
-}

substitute :: Var -> Exp -> Exp -> Exp
substitute varS valS exp = case exp of
  (EApp exp1 exp2) -> EApp (sub exp1) (sub exp2)
  (EIf cond true false) -> EIf (sub cond) (sub true) (sub false)
  (ELam exp var) -> ELam (sub exp) var
  (EOp op exp1 exp2) -> EOp op (sub exp1) (sub exp2)
  (EVar var) | var == varS -> valS
  var @ (EVar _) -> var
  val @ (EVal _) -> val
  where sub = substitute varS valS

helpEvalExp :: Exp -> Reader Environment (Err Exp)
-- helpEvalExp exp | trace ("=========\n" ++ (show exp)) False = undefined
helpEvalExp (EIf condExp trueExp falseExp) = do
  cond <- helpEvalExp condExp
  case cond of
    Bad err -> return $ Bad err
    Ok (EVal (TBool bool)) -> helpEvalExp $ if bool then trueExp else falseExp
    Ok value -> return $ Bad $ show $ AppTypeMismatch (FuncVar "if") ([value])

helpEvalExp (ELet defs exp) =
  local (addBindings defs) (helpEvalExp exp)
  where
    addBindings :: [(Var, Exp)] -> Environment -> Environment
    addBindings defs env = foldl (flip (uncurry addBinding)) env defs

helpEvalExp lambda @ (ELam _ _) = return $ Ok lambda 

helpEvalExp var @ (EVar ident) = do
  env <- ask
  return $ case Map.lookup ident (bindings env) of
    Nothing ->
      if (resolve_values env)
      then Bad $ show $ UnboundVar ident env
      else Ok $ var
    Just value -> Ok $ value
  
helpEvalExp (EOp op left right) = do
  leftEval <- helpEvalExp left
  rightEval <- helpEvalExp right
  return $ (liftM2 pair leftEval rightEval) >>= (uncurry (evalOpL op))
  
helpEvalExp (EApp funcExp valueExp) = do
  funcErr <- local turn_off_resolving (helpEvalExp funcExp)
  case funcErr of
    error @ (Bad _)  -> return error 
    Ok (ELam fun ident) -> (helpEvalExp (substitute ident valueExp fun))
    Ok value            -> return $ Bad $ show $ WrongType value valueExp

helpEvalExp value @ (EVal _ ) = return $ Ok value

pair :: a -> b -> (a, b)
pair a b = (a, b)


evalOpL :: Op -> (Exp -> Exp -> Err Exp)
evalOpL op (EVal a) (EVal b) = EVal `fmap` (evalOp op a b)
evalOpL op exp1 exp2 = Bad $ show $ WrongType exp1 (EOp op exp1 exp2)

-- TODO consider instance of appplicative
-- TODO operators are translated to functions: + ==> _plus
evalOp :: Op -> (DynVal -> DynVal -> Err DynVal)
evalOp OpAdd (TInt a) (TInt b) = Ok $ TInt (a + b)
evalOp OpMul (TInt a) (TInt b) = Ok $ TInt (a * b)
evalOp OpSub (TInt a) (TInt b) = Ok $ TInt (a - b)
evalOp OpAnd (TBool a) (TBool b) = Ok $ TBool (a && b) 
evalOp OpOr  (TBool a) (TBool b) = Ok $ TBool (a || b)
evalOp OpLes (TInt a) (TInt b) = Ok $ TBool (a < b)
-- evalOp OpGre (TInt a) (TInt b) = Ok $ TBool (a > b)
evalOp OpEqu (TInt a) (TInt b) = Ok $ TBool (a == b)
evalOp op arg1 arg2 = Bad $ show $ AppTypeMismatch (FuncOp op) [EVal arg1, EVal arg2]

{-                     INSTANCES                      -}


instance Show DynVal where
  show (TInt int) = show int
  show (TBool bool) = show bool

instance Eq DynVal where
  (==) (TInt  a) (TInt  b) = (a == b)
  (==) (TBool a) (TBool b) = (a == b)
  (==) _         _         = False
