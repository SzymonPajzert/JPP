{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -fdefer-typed-holes #-}

module DynGrammar (interpret, DynVal(..), Exp(..)) where


import Prelude hiding (exp, error, unlines)
import qualified Data.Map.Strict as Map
import Control.Monad.Reader

import Exp
import Err

data Binds
  = Strict DynVal
  | Lazy Exp
  deriving (Show)
  
data Environment = Environment
  { bindings :: Map.Map Var Exp
  , resolve_values :: Bool
  } deriving (Show)

type RunErr = Err RuntimeError

data RuntimeError
  = AppTypeMismatch FuncId [Exp]
  | UnboundVar Var Environment
  | WrongType Exp Exp
  deriving (Show)

data FuncId
  = FuncVar Var
  | FuncOp  Op
  deriving (Show)


interpret :: Exp -> RunErr Exp
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
  (ELet defs exp) -> ELet defs (sub exp)
  (EApp exp1 exp2) -> EApp (sub exp1) (sub exp2)
  (EIf cond true false) -> EIf (sub cond) (sub true) (sub false)
  (ELam exp var) -> ELam (sub exp) var
  (EOp op exp1 exp2) -> EOp op (sub exp1) (sub exp2)
  (EVar var) | var == varS -> valS
  var @ (EVar _) -> var
  val @ (EVal _) -> val
  where sub = substitute varS valS

helpEvalExp :: Exp -> Reader Environment (RunErr Exp)
helpEvalExp (EIf condExp trueExp falseExp) = do
  cond <- helpEvalExp condExp
  case cond of
    Bad err -> return $ Bad err
    Ok (EVal (TBool bool)) -> helpEvalExp $ if bool then trueExp else falseExp
    Ok value -> return $ Bad $ AppTypeMismatch (FuncVar "if") ([value])

helpEvalExp (ELet defs exp) =
  local (addBindings defs) (helpEvalExp exp)
  where
    addBindings :: [(Var, Exp)] -> Environment -> Environment
    addBindings defs env = foldl (flip (uncurry addBinding)) env defs

helpEvalExp lambda @ (ELam _ _) = return $ Ok lambda 

helpEvalExp var @ (EVar ident) = do
  env <- ask
  case Map.lookup ident (bindings env) of
    Nothing ->
      if (resolve_values env)
      then return $ Bad $ UnboundVar ident env
      else return $ Ok $ var
    Just value -> helpEvalExp value
  
helpEvalExp (EOp op left right) = do
  leftEval <- helpEvalExp left
  rightEval <- helpEvalExp right
  return $ (liftM2 pair leftEval rightEval) >>= (uncurry (evalOpL op))
  
helpEvalExp (EApp funcExp valueExp) = do
  funcErr <- helpEvalExp funcExp
  case funcErr of
    error @ (Bad _)  -> return error 
    Ok (ELam fun ident) -> (helpEvalExp (substitute ident valueExp fun))
    Ok value            -> return $ Bad $ WrongType value valueExp

helpEvalExp value @ (EVal _ ) = return $ Ok value

pair :: a -> b -> (a, b)
pair a b = (a, b)


evalOpL :: Op -> (Exp -> Exp -> RunErr Exp)
evalOpL op (EVal a) (EVal b) = EVal `fmap` (evalOp op a b) 
evalOpL op exp1 exp2 = Bad $  WrongType exp1 (EOp op exp1 exp2) 

-- TODO consider instance of appplicative
-- TODO operators are translated to functions: + ==> _plus
evalOp :: Op -> (DynVal -> DynVal -> RunErr DynVal)
evalOp OpAdd (TInt a) (TInt b) = Ok $ TInt (a + b)
evalOp OpMul (TInt a) (TInt b) = Ok $ TInt (a * b)
evalOp OpSub (TInt a) (TInt b) = Ok $ TInt (a - b)
evalOp OpAnd (TBool a) (TBool b) = Ok $ TBool (a && b) 
evalOp OpOr  (TBool a) (TBool b) = Ok $ TBool (a || b)
evalOp OpLes (TInt a) (TInt b) = Ok $ TBool (a < b)
-- evalOp OpGre (TInt a) (TInt b) = Ok $ TBool (a > b)
evalOp OpEqu (TInt a) (TInt b) = Ok $ TBool (a == b)
evalOp op arg1 arg2 = Bad $ AppTypeMismatch (FuncOp op) [EVal arg1, EVal arg2]
