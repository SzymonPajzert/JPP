{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -fdefer-typed-holes #-}

module DynGrammar (interpret, Exp(..)) where


import Prelude hiding (exp, error, unlines)
import qualified Data.Map.Strict as Map
import Control.Monad.Reader

import Exp
import Err
import Util (unlines)

newtype Bindings = Bindings { getMap :: (Map.Map Var Exp) }

instance Show Bindings where
  show (Bindings bindMap) = unlines $ map represent (Map.toList bindMap)
    where represent (var, exp) = var ++ " -> " ++ (show exp)
  
data Environment = Environment
  { bindings :: Bindings
-- TODO remove  , resolve_values :: Bool
  } deriving (Show)

type RunErr = Err RuntimeError

data RuntimeError
  = AppTypeMismatch FuncId [Exp]
  | UnboundVar Var Environment
  | WrongType Exp Exp
  | MatchError
  deriving (Show)

data FuncId
  = FuncVar Var
  | FuncOp  Op
  deriving (Show)


interpret :: Exp -> RunErr Exp
interpret exp = runReader (helpEvalExp exp) env
  where
    baseMap = Map.fromList [("True", EBool True),
                            ("False", EBool False)
                           ]
    env = Environment (Bindings baseMap)
                      

addBinding :: Var -> Exp -> Environment -> Environment
addBinding var exp env = env {bindings = Bindings (Map.insert var exp (getMap $ bindings env)) }

getBinding :: Var -> Reader Environment (Maybe Exp)
getBinding ident = do
  bindMap <- asks (getMap . bindings)
  return $ Map.lookup ident bindMap


{- TODO remove
turn_off_resolving :: Environment -> Environment
turn_off_resolving env = env { resolve_values = False }
-}

safeZip :: [a] -> [b] -> Maybe [(a, b)]
safeZip (a:as) (b:bs) = fmap ((:) (a, b)) $ safeZip as bs
safeZip []     []     = Just []
safeZip _      _      = Nothing

bindValues :: Bind -> Exp -> Maybe [(Var, Exp)]
bindValues BIgnore      _   = Just []
bindValues (BVar var)   exp = Just [(var, exp)]
bindValues (BTup binds) (ETup exps) = do
  pairs <- safeZip binds exps
  toConcat <- sequence $ map (uncurry bindValues) pairs
  return $ concat toConcat
bindValues (BTup _)     _   = Nothing

tryBinds :: [Bind] -> Exp -> Maybe [(Var, Exp)]
tryBinds []     _   = Nothing
tryBinds (b:bs) exp = case bindValues b exp of
  Nothing -> tryBinds bs exp
  result  -> result


substitute :: Var -> Exp -> Exp -> Exp
substitute varS valS exp = case exp of
  (ELet defs mainExp) -> ELet defs (sub mainExp)
  (EApp exp1 exp2) -> EApp (sub exp1) (sub exp2)
  (EIf cond true false) -> EIf (sub cond) (sub true) (sub false)
  (ELam lamExp var) -> ELam (sub lamExp) var
  (EOp op exp1 exp2) -> EOp op (sub exp1) (sub exp2)
  (EVar var) | var == varS -> valS
  (ETup tuple) -> ETup (map sub tuple)
  var @ (EVar _) -> var
  val @ (EInt _) -> val
  val @ (EBool _) -> val
  where sub = substitute varS valS

helpEvalExp :: Exp -> Reader Environment (RunErr Exp)
helpEvalExp (EIf condExp trueExp falseExp) = do
  cond <- helpEvalExp condExp
  case cond of
    Bad err -> return $ Bad err
    Ok (EBool bool) -> helpEvalExp $ if bool then trueExp else falseExp
    Ok value -> return $ Bad $ AppTypeMismatch (FuncVar "if") ([value])

helpEvalExp (ELet defs exp) =
  local (addBindings defs) (helpEvalExp exp)
  where
    addBindings :: [(Var, Exp)] -> Environment -> Environment
    addBindings anyDefs env = foldl (flip (uncurry addBinding)) env anyDefs

helpEvalExp lambda @ (ELam _ _) = return $ Ok lambda 

helpEvalExp var @ (EVar ident) = do
  bindMaybe <- getBinding ident
  case bindMaybe of
    Nothing -> do
      env <- ask
      return $ Bad $ UnboundVar ident env
    Just value -> helpEvalExp value
  
helpEvalExp (EOp op left right) = do
  leftEval <- helpEvalExp left
  rightEval <- helpEvalExp right
  return $ (liftM2 pair leftEval rightEval) >>= (uncurry (evalOp op))
  
helpEvalExp (EApp funcExp valueExp) = do
  funcErr <- helpEvalExp funcExp
  case funcErr of
    error @ (Bad _)  -> return error 
    Ok (ELam fun bind) -> case (bindValues bind valueExp) of
      Nothing        -> return $ Bad MatchError
      Just valPairs  ->
        let subExp = foldl (flip (uncurry substitute)) fun valPairs
        in helpEvalExp subExp
    Ok value           -> return $ Bad $ WrongType value valueExp

helpEvalExp (ETup tuple) = do
  first <- sequence $ map helpEvalExp tuple
  return $ ETup `fmap` sequence first

helpEvalExp value = return $ Ok value

pair :: a -> b -> (a, b)
pair a b = (a, b)

-- TODO consider instance of appplicative
-- TODO operators are translated to functions: + ==> _plus
evalOp :: Op -> (Exp -> Exp -> RunErr Exp)
evalOp OpAdd (EInt a) (EInt b) = Ok $ EInt (a + b)
evalOp OpMul (EInt a) (EInt b) = Ok $ EInt (a * b)
evalOp OpSub (EInt a) (EInt b) = Ok $ EInt (a - b)
evalOp OpAnd (EBool a) (EBool b) = Ok $ EBool (a && b) 
evalOp OpOr  (EBool a) (EBool b) = Ok $ EBool (a || b)
evalOp OpLes (EInt a) (EInt b) = Ok $ EBool (a < b)
evalOp OpEqu (EInt a) (EInt b) = Ok $ EBool (a == b)
evalOp op arg1 arg2 = Bad $ AppTypeMismatch (FuncOp op) [arg1, arg2]
