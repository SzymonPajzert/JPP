module Exp where

import Prelude hiding (exp)
import Data.Function (on)
import qualified Data.Map as Map
import Control.Monad.Reader

type Var = String
type Def = (Var, Exp)

data Op
  = OpAdd
  | OpMul
  | OpSub
  | OpLes
  | OpEqu
  deriving (Show)

data Bind
  = BIgnore
  | BInt Integer
  | BVar Var             -- variable binding (TODO remove and merger with tuple)
  | BTup [Bind]          -- tuple binding
  | BPol String [Bind]   -- polymorphic type binding
  deriving (Show)

data Exp
  = EApp Exp Exp
  | ELet [Def] Exp
  | ELam Exp Bind
  | EOp Op Exp Exp
  | EVar Var
  | EInt Integer
  | ETup [Exp]
  | EMat Exp [(Bind, Exp)]
  | ECon Var [Exp]
  deriving (Show)

is_basic :: Exp -> Bool
is_basic exp = case exp of
  EInt  _  -> True
  ETup  _  -> True
  ECon _ _ -> True
  _        -> False

instance Eq Exp where
  (==) (EInt  a) (EInt  b) = (a == b)
  (==) (ETup  a) (ETup  b) = (a == b)
  (==) (ECon id1 par1) (ECon id2 par2) = (id1 == id2) && (par1 == par2)
  (==) _         _         = False


--              higher types            --

type ExpEnv = (Exp, Environment)

newtype Bindings = Bindings { getMap :: (Map.Map Var ExpEnv) }

instance Show Bindings where
  show (Bindings bindMap) = unlines $ map represent (Map.toList bindMap)
    where represent (var, exp) = var ++ " -> " ++ (show exp)

data Environment = Environment
  { bindings :: Bindings } deriving (Show)

emptyEnv :: Environment
emptyEnv = Environment $ Bindings Map.empty

type CompiledProgram = (Exp, Exp.Environment)

-- | Merge two bindings, first one has precedence in case of conflicts
mergeBindings :: Bindings -> Bindings -> Bindings
mergeBindings bind1 bind2 = Bindings $ (getMap bind1) `Map.union` (getMap bind2)

-- | Merge two environments, first one has precedence in case of conflicts
mergeEnv :: Environment -> Environment -> Environment
mergeEnv env1 env2 = Environment $ ((mergeBindings `on` bindings) env1 env2)

addBinding :: Var -> ExpEnv -> Environment -> Environment
addBinding var exp env = env {
  bindings = Bindings (Map.insert var exp (getMap $ bindings env))
  }

addBindings :: [(Var, Exp)] -> Environment -> Reader Environment Environment
addBindings anyDefs env = do
  boundAnyDefs <- mapM bindEnv anyDefs
  return $ foldl (flip (uncurry addBinding)) env boundAnyDefs
  where
    bindEnv (var, exp) = return $ (var, (exp, env))

--                 consts               --

truePol :: String
truePol = "True"

true :: Exp
true = ECon truePol []

falsePol :: String
falsePol = "False"

false :: Exp
false = ECon falsePol []

emptyList :: String
emptyList = "_Nil"

listCons :: String
listCons  = "_Cons"
