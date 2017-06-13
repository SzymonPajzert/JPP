{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -fdefer-typed-holes #-}

module DynGrammar (interpret, Exp(..)) where


import Prelude hiding (exp, error, unlines)
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Debug.Trace

import Exp 
import Err 
import Util (unlines, indent)

type ExpEnv = (Exp, Environment)

newtype Bindings = Bindings { getMap :: (Map.Map Var ExpEnv) }

type EvalResult = Reader Environment (RunErr ExpEnv)

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
  | MatchError Exp [Bind]
  deriving (Show)

data FuncId
  = FuncVar Var
  | FuncOp  Op
  deriving (Show)

type MaybeBind =  RunErr (Maybe ([(Var, Exp)], Environment))

emptyEnv :: Environment
emptyEnv = Environment $ Bindings Map.empty

interpret :: Exp -> RunErr Exp
interpret exp = runReader (evalExp exp) env >>= (return . fst)
  where
    baseMap = Map.fromList [("True", (EBool True, emptyEnv)),
                            ("False", (EBool False, emptyEnv))
                           ]
    env = Environment (Bindings baseMap)
                      

addBinding :: Var -> ExpEnv -> Environment -> Environment
addBinding var exp env = env {bindings = Bindings (Map.insert var exp (getMap $ bindings env)) }

addBindings :: [(Var, Exp)] -> Environment -> Reader Environment Environment
addBindings anyDefs env = do
  boundAnyDefs <- mapM bindEnv anyDefs
  return $ foldl (flip (uncurry addBinding)) env boundAnyDefs
  where
    bindEnv (var, exp) = return $ (var, (exp, env))

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

bindValues :: Bind -> Exp -> Reader Environment MaybeBind
bindValues BIgnore      _   = return $ Ok $ Just ([], emptyEnv)
bindValues (BVar var)   exp = return $ Ok $ Just ([(var, exp)], emptyEnv)
bindValues bind lazyExp = do
  expError <- evalExp lazyExp
  case expError of
    Bad error -> return $ Bad error
    -- possibly have to save the environment
    Ok (exp, env) -> case (bind, exp) of
      ((BTup binds), (ETup exps)) -> bindLists binds exps env
      ((BTup _)    , _          ) -> return $ Ok $ Nothing
      ((BInt int)  ,(EInt value)) -> return $ Ok $
        if int == value then Just ([], emptyEnv) else Nothing
      ((BInt _)    , _          ) -> return $ Ok $ Nothing
      ((BPol nameP binds), (ECon nameC exps)) ->
        if nameP == nameC
        then bindLists binds exps env
        else return $ Ok $ Nothing
      ((BPol _ _) , _          ) -> return $ Ok $ Nothing

bindLists :: [Bind] -> [Exp] -> Environment -> Reader Environment MaybeBind
bindLists binds exps env =
  case safeZip binds exps of
    Nothing -> return $ Ok $ Nothing
    Just pairs -> do
      extract <- mapM (uncurry bindValues) pairs
      case sequence extract of
        Bad error -> return $ Bad error
        Ok seq -> case sequence seq of
          Nothing -> return $ Ok Nothing
          Just result -> do
            let (values, envs) = unzip result
            let finalEnv = foldl mergeEnv env envs
            return $ Ok $ Just (concat values, finalEnv) 
          
                     
tryBinds :: [(Bind, Exp)] -> Exp -> Reader Environment (RunErr (Maybe (([(Var, Exp)], Environment), Exp)))
tryBinds []     _   = return $ Ok $ Nothing
tryBinds ((b,cont):bs) exp = do
  bound <- bindValues b exp
  case bound of
    Bad error -> return $ Bad error
    Ok Nothing -> tryBinds bs exp
    Ok (Just result)  -> return $ Ok $ Just (result, cont)

unpack :: (RunErr (Maybe (a, b))) -> (RunErr (Maybe a), RunErr (Maybe b))
unpack (Bad error) = (Bad error, Bad error)
unpack (Ok Nothing) = (Ok Nothing, Ok Nothing)
unpack (Ok (Just (a, b))) = (Ok $ Just a, Ok $ Just b) 

substitute :: Var -> Exp -> Exp -> Exp
substitute varS valS exp = case exp of
  (ELet defs mainExp) -> ELet defs (sub mainExp)
  (EApp exp1 exp2) -> EApp (sub exp1) (sub exp2)
  (EIf cond true false) -> EIf (sub cond) (sub true) (sub false)
  (ELam lamExp var) -> ELam (sub lamExp) var
  (EOp op exp1 exp2) -> EOp op (sub exp1) (sub exp2)
  (EVar var) | var == varS -> valS
  (ETup tuple) -> ETup (map sub tuple)
  (EMat expr binds) -> EMat (sub expr) (map (\(bind, exp) -> (bind, sub exp)) binds)
  (ECon name elems) -> ECon name (map sub elems)
  var @ (EVar _) -> var
  val @ (EInt _) -> val
  val @ (EBool _) -> val
  where sub = substitute varS valS

substituteMany :: [(Var, Exp)] -> Exp -> Exp
substituteMany binds expr = foldl (flip (uncurry substitute)) expr binds

substituteBind :: Environment -> Exp -> MaybeBind -> RuntimeError -> EvalResult
substituteBind env expr bound matchError =
  case bound of
    Bad error      -> return $ Bad error
    Ok Nothing     -> return $ Bad $ matchError
    Ok (Just (valPairs, environment))  -> do
      let subExp = substituteMany valPairs expr
      local (mergeEnv environment . mergeEnv env) (evalExp subExp)

evalExp :: Exp -> EvalResult
-- evalExp value | trace ((indent value) ++ "----------------\n\n\n") False = undefined
evalExp (EIf condExp trueExp falseExp) = do
  cond <- evalExp condExp
  case cond of
    Bad err -> return $ Bad err
    Ok ((EBool bool), _) -> evalExp $ if bool then trueExp else falseExp
    Ok (value, _) -> return $ Bad $ AppTypeMismatch (FuncVar "if") ([value])

evalExp (ELet defs exp) = do
  env <- ask
  
  newEnv <- addBindings defs env

  evalPair <- local (mergeEnv newEnv) (evalExp exp)
  case evalPair of
    Ok (evalExp, _) -> return $ Ok (evalExp, newEnv)
    err @ (Bad _)  -> return err

evalExp lambda @ (ELam _ _) = do
  env <- ask
  return $ Ok (lambda, env)

evalExp (EVar ident) = do
  bindMaybe <- getBinding ident
  case bindMaybe of
    Nothing -> do
      env <- ask
      return $ Bad $ UnboundVar ident env
    Just (value, env) -> local (mergeEnv env) (evalExp value)
  
evalExp (EOp op left right) = do
  leftEval <- evalExp left
  rightEval <- evalExp right
  env <- ask
  let evalValue = (liftM2 pair (fmap fst leftEval) (fmap fst rightEval)) >>= (uncurry $ evalOp op)

  case evalValue of
    Ok value      -> return $ Ok (value, env)
    Bad err       -> return $ Bad err
  
evalExp (EApp funcExp valueExp) = do
  funcErr <- evalExp funcExp
  case funcErr of
    error @ (Bad _)  -> return error
    Ok ((ELam fun bind), env) -> do
      bound <- bindValues bind valueExp
      substituteBind env fun bound (MatchError valueExp [bind])
    Ok (value, _)             -> return $ Bad $ WrongType value valueExp

evalExp (EMat mExp binds) = do
  mExpMaybe <- evalExp mExp
  case mExpMaybe of
    Bad error -> return $ Bad error
    Ok (mExpEval, expEnv) -> do
      boundPair <- tryBinds binds mExpEval
      let (bound, contMaybe) = unpack boundPair
      let matchError = (MatchError mExpEval (map fst binds))

      case contMaybe of
        Bad error -> return $ Bad error
        Ok Nothing -> return $ Bad matchError
        Ok (Just cont) -> substituteBind expEnv cont bound matchError
        
evalExp (ETup tuple) = helpEvalList tuple ETup
evalExp (ECon name params) = helpEvalList params (ECon name)
evalExp value @ (EInt  _) = return $ Ok (value, emptyEnv)
evalExp value @ (EBool _) = return $ Ok (value, emptyEnv)

helpEvalList :: [Exp] -> ([Exp] -> Exp) -> EvalResult 
helpEvalList list packing = do
  env <- ask
  return $ Ok $ (packing list, env)

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
