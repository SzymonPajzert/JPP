{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -fdefer-typed-holes #-}

module DynamicGrammar where

import Prelude hiding (exp, error)
import Data.List (partition)

import qualified Data.Map.Strict as Map
import qualified AbsGrammar as Abs
import ParGrammar
import LexGrammar
import Control.Monad.Reader
-- import AbsGrammar (LIdent)

import ErrM

type Program = [Abs.TopDef]

type ParseFun a = [Token] -> Err a

type Var = String
type Def = (Var, Exp)

instance Show DynVal where
  show (TInt int) = show int
  show (TBool bool) = show bool
  show (TUnapp _) = "fun"

instance Eq DynVal where
  (==) (TInt  a) (TInt  b) = (a == b)
  (==) (TBool a) (TBool b) = (a == b)
  (==) _         _         = False

data DynVal
  = TInt Integer
  | TBool Bool
  | TUnapp (DynVal -> DynVal)
--  | TTuple [DynVal]
--  identyfikatory typów polimorficznych (listy i booleany będą tutaj)

data Exp
  = EApp Exp Exp
  | EIf Exp Exp Exp
  | ELet [Def] Exp
  | ELam Exp Var
  | EInt Integer
  | EOp Op Exp Exp
  | EVar Var
  deriving (Show)

type Bindings = Map.Map Var DynVal

data RuntimeError
  = AppTypeMismatch FuncId [DynVal]
  | UnsupportedMultipleLet
  | UnboundVar Var Bindings
  | WrongType DynVal
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
desugar (Abs.EAdd left right) = (liftM2 $ EOp OpAdd) (desugar left) (desugar right)
desugar (Abs.ESub left right) = (liftM2 $ EOp OpSub) (desugar left) (desugar right)
desugar (Abs.EMul left right) = (liftM2 $ EOp OpMul) (desugar left) (desugar right)
-- desugar (Abs.Cons _ _)
desugar (Abs.EApp func value) = (liftM2 EApp) (desugar func) (desugar value)
-- desugar (ELis _)
-- desugar (ETup _)
desugar (Abs.EInt int) = Ok $ EInt int
desugar (Abs.ECon (Abs.UIdent ident)) = Ok $ EVar ident
desugar (Abs.EVar (Abs.Ident ident)) = Ok $ EVar ident
desugar (Abs.ETup [exp]) = desugar exp


desugar unsupported = Bad $ show $ UnsupportedSyntaxExpr unsupported



interpret :: Exp -> Err DynVal
interpret exp = runReader (helpEvalExp exp) baseMap
  where
    baseMap = Map.fromList [("True", TBool True),
                            ("False", TBool False)
                           ]
                      

addBinding :: Var -> DynVal -> Bindings -> Bindings
addBinding = Map.insert

-- todo really remove
unsafeExt :: Err a -> a
unsafeExt (Ok res) = res

trans :: (a -> Reader r b) -> Reader r (a -> b)
trans f = do
    r <- ask
    return $ \a -> runReader (f a) r

helpEvalExp :: Exp -> Reader Bindings (Err DynVal)
helpEvalExp (EInt n) = return $ Ok $ TInt n
helpEvalExp (EIf condExp trueExp falseExp) = do
  cond <- helpEvalExp condExp
  case cond of
    Bad err -> return $ Bad err
    Ok (TBool bool) -> helpEvalExp $ if bool then trueExp else falseExp
    Ok value -> return $ Bad $ show $ AppTypeMismatch (FuncVar "if") ([value])

helpEvalExp (ELet [(ident, value)] exp) = do
  evalAssigned <- helpEvalExp value
  case evalAssigned of
    Ok res -> local (addBinding ident res) (helpEvalExp exp)
    other -> return other
  
helpEvalExp (ELet [] exp) = helpEvalExp exp
helpEvalExp (ELet _ _) = return $ Bad $ show $ UnsupportedMultipleLet

-- DynVal -> Reader Bindings (Err DynVal

helpEvalExp (ELam exp ident) = do
  r <- ask
  return $ Ok $ TUnapp $ (\res -> unsafeExt $ runReader (local (addBinding ident res) (helpEvalExp exp)) r)

helpEvalExp (EVar ident) = do
  values <- ask
  return $ case Map.lookup ident values of
    Nothing -> Bad $ show $ UnboundVar ident values
    Just value -> Ok $ value
  
helpEvalExp (EOp op left right) = do
  leftEval <- helpEvalExp left
  rightEval <- helpEvalExp right
  return $ (liftM2 pair leftEval rightEval) >>= (uncurry (evalOp op))


helpEvalExp (EApp funcExp valueExp) = do
  function <- helpEvalExp funcExp
  argValue <- helpEvalExp valueExp
  
  return $ case function of
    Ok (TUnapp func) -> liftM func argValue
    value -> case value of
      Ok val ->  Bad $ show $ WrongType val
      error -> error


pair :: a -> b -> (a, b)
pair a b = (a, b)

data Op
  = OpAdd
  | OpMul
  | OpSub
  | OpAnd
  | OpOr
  | OpLes
  | OpEqu
  deriving (Show)

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
evalOp op arg1 arg2 = Bad $ show $ AppTypeMismatch (FuncOp op) [arg1, arg2]



