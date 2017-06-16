{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -fdefer-typed-holes #-}

module DynGrammar (interpret, Exp(..)) where


import Prelude hiding (exp, error, unlines, seq, pred)
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Debug.Trace (trace)

import Exp
import Err
import Util (safeZip, indent)

type EvalResult = EnvErr ExpEnv

type RunErr = Err (RuntimeError, Environment)

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

type MaybeBind =  Maybe ([(Var, Exp)], Environment)

type EnvErr a = Reader Environment (RunErr a)

returnError :: RuntimeError -> Reader Environment (RunErr a)
returnError err = do
  env <- ask
  return $ Bad $ (err, env)

interpret :: CompiledProgram -> RunErr Exp
interpret (exp, env) = runReader (evalExp exp) env >>= (return . fst)

getBinding :: Var -> Reader Environment (Maybe ExpEnv)
getBinding ident = do
  bindMap <- asks (getMap . bindings)
  return $ Map.lookup ident bindMap

bindValues :: Bind -> Exp -> EnvErr MaybeBind
bindValues bind lazyExp = do
  expError <- evalExp lazyExp
  case expError of
    Bad error -> return $ Bad error
    -- possibly have to save the environment
    Ok (exp, env) -> case (bind, exp) of
      (BIgnore     , _ )          -> return $ Ok $ Just ([], emptyEnv)
      ((BVar var)  , valueExp)    -> return $ Ok $ Just ([(var, valueExp)], emptyEnv)
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

bindLists :: [Bind] -> [Exp] -> Environment -> EnvErr MaybeBind
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


tryBinds :: [(Bind, Exp)] -> Exp -> EnvErr (Maybe (([(Var, Exp)], Environment), Exp))
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
  (ELam lamExp var) -> ELam (sub lamExp) var
  (EOp op exp1 exp2) -> EOp op (sub exp1) (sub exp2)
  (EVar var) | var == varS -> valS
  (ETup tuple) -> ETup (map sub tuple)
  (EMat expr binds) -> EMat (sub expr) (map (\(bind, expBind) -> (bind, sub expBind)) binds)
  (ECon name elems) -> ECon name (map sub elems)
  var @ (EVar _) -> var
  val @ (EInt _) -> val
  where sub = substitute varS valS

substituteMany :: [(Var, Exp)] -> Exp -> Exp
substituteMany binds expr = foldl (flip (uncurry substitute)) expr binds

substituteBind :: Environment -> Exp -> (RunErr MaybeBind) -> RuntimeError -> EvalResult
substituteBind env expr bound matchError =
  case bound of
    Bad error      -> return $ Bad error
    Ok Nothing     -> returnError $ matchError
    Ok (Just (valPairs, environment))  -> do
      let subExp = substituteMany valPairs expr
      local (mergeEnv environment . mergeEnv env) (evalExp subExp)

evalExp :: Exp -> EvalResult
-- evalExp value | trace ((indent value) ++ "\n----------------\n\n") False = undefined

evalExp (ELet defs exp) = do
  env <- ask
  newEnv <- addBindings defs env

  evalPair <- local (mergeEnv newEnv) (evalExp exp)
  case evalPair of
    Ok (evalExpValue, _) -> return $ Ok (evalExpValue, newEnv)
    err @ (Bad _)  -> return err

evalExp lambda @ (ELam _ _) = do
  env <- ask
  return $ Ok (lambda, env)

evalExp (EVar ident) = do
  bindMaybe <- getBinding ident
  case bindMaybe of
    Nothing -> do
      env <- ask
      returnError $ UnboundVar ident env
    Just (value, env) -> local (mergeEnv env) (evalExp value)

-- TODO make nicer
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
    Ok (value, _)             -> returnError $ WrongType value valueExp

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
        Ok Nothing -> returnError matchError
        Ok (Just cont) -> substituteBind expEnv cont bound matchError

evalExp (ETup tuple) = helpEvalList tuple ETup
evalExp (ECon name params) = helpEvalList params (ECon name)
evalExp value @ (EInt  _) = return $ Ok (value, emptyEnv)

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
evalOp OpLes (EInt a) (EInt b) = Ok $ encode (a < b)
evalOp OpEqu exp1 exp2 = Ok $ encode (exp1 == exp2)
evalOp op arg1 arg2 = Bad $ (AppTypeMismatch (FuncOp op) [arg1, arg2], emptyEnv)


encode :: Bool -> Exp
encode pred = if pred then true else false
