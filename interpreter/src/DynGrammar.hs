{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -fdefer-typed-holes #-}

module DynGrammar (interpret, Exp(..)) where


import Prelude hiding (exp, error, unlines)
import qualified Data.Map.Strict as Map
import Control.Monad.Reader

import Exp
import Err
import Util (unlines)

type ExpEnv = (Exp, Environment)

newtype Bindings = Bindings { getMap :: (Map.Map Var ExpEnv) }

instance Show Bindings where
  show (Bindings bindMap) = unlines $ map represent (Map.toList bindMap)
    where represent (var, exp) = var ++ " -> " ++ (show exp)
  
data Environment = Environment
  { bindings :: Bindings } deriving (Show)

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

emptyEnv :: Environment
emptyEnv = Environment $ Bindings Map.empty

interpret :: Exp -> RunErr Exp
interpret exp = runReader (helpEvalExp exp) env >>= (return . fst)
  where
    baseMap = Map.fromList [("True", (EBool True, emptyEnv)),
                            ("False", (EBool False, emptyEnv))
                           ]
    env = Environment (Bindings baseMap)
                      

addBinding :: Var -> ExpEnv -> Environment -> Environment
addBinding var exp env = env {bindings = Bindings (Map.insert var exp (getMap $ bindings env)) }

getBinding :: Var -> Reader Environment (Maybe ExpEnv)
getBinding ident = do
  bindMap <- asks (getMap . bindings)
  return $ Map.lookup ident bindMap

-- | Merge two environments, first one has precedence in case of conflicts
mergeEnv :: Environment -> Environment -> Environment
mergeEnv env1 env2 = Environment $ Bindings ((getMap $ bindings env1) `Map.union` (getMap $ bindings env2))

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

helpEvalExp :: Exp -> Reader Environment (RunErr ExpEnv)
helpEvalExp (EIf condExp trueExp falseExp) = do
  cond <- helpEvalExp condExp
  case cond of
    Bad err -> return $ Bad err
    Ok ((EBool bool), _) -> helpEvalExp $ if bool then trueExp else falseExp
    Ok (value, _) -> return $ Bad $ AppTypeMismatch (FuncVar "if") ([value])

helpEvalExp (ELet defs exp) = do
  env <- ask

  let bindEnv (var, exp) = (var, (exp, env))
  let addBindings anyDefs env = foldl (flip (uncurry addBinding)) env (map bindEnv anyDefs)
  let newEnv = addBindings defs env

  evalPair <- local (mergeEnv newEnv) (helpEvalExp exp)
  case evalPair of
    Ok (evalExp, _) -> return $ Ok (evalExp, newEnv)
    err @ (Bad _)  -> return err

helpEvalExp lambda @ (ELam _ _) = do
  env <- ask
  return $ Ok (lambda, env)

helpEvalExp (EVar ident) = do
  bindMaybe <- getBinding ident
  case bindMaybe of
    Nothing -> do
      env <- ask
      return $ Bad $ UnboundVar ident env
    Just (value, env) -> local (mergeEnv env) (helpEvalExp value)
  
helpEvalExp (EOp op left right) = do
  leftEval <- helpEvalExp left
  rightEval <- helpEvalExp right
  env <- ask
  let evalValue = (liftM2 pair (fmap fst leftEval) (fmap fst rightEval)) >>= (uncurry $ evalOp op)

  case evalValue of
    Ok value      -> return $ Ok (value, env)
    Bad err       -> return $ Bad err
  
helpEvalExp (EApp funcExp valueExp) = do
  funcErr <- helpEvalExp funcExp
  case funcErr of
    error @ (Bad _)  -> return error 
    Ok ((ELam fun bind), env) -> case (bindValues bind valueExp) of
      Nothing        -> return $ Bad MatchError
      Just valPairs  ->
        let subExp = foldl (flip (uncurry substitute)) fun valPairs
        in local (mergeEnv env) (helpEvalExp subExp)
    Ok (value, _)             -> return $ Bad $ WrongType value valueExp

helpEvalExp (ETup tuple) = do
  first <- sequence $ map helpEvalExp tuple
  return $ mapTup `fmap` sequence first
  where
    mapTup []                  = (ETup [], emptyEnv)
    mapTup ((first, env):rest) = (ETup (first:newRest), env)
      where newRest = map fst rest


helpEvalExp value = return $ Ok (value, emptyEnv)

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
