module Exp where

import Prelude hiding (exp)

type Var = String
type Def = (Var, Exp)

data Op
  = OpAdd
  | OpMul
  | OpSub
  | OpAnd
  | OpOr
  | OpLes
  | OpEqu
  deriving (Show)

data Exp
  = EApp Exp Exp
  | EIf Exp Exp Exp
  | ELet [Def] Exp
  | ELam Exp Var
  | EOp Op Exp Exp
  | EVar Var
  | EInt Integer
  | EBool Bool
  | ETup [Exp]
  deriving (Show)

is_basic :: Exp -> Bool
is_basic exp = case exp of
  EInt  _ -> True
  EBool _ -> True
  ETup  _ -> True
  _       -> False

instance Eq Exp where
  (==) (EInt  a) (EInt  b) = (a == b)
  (==) (EBool a) (EBool b) = (a == b)
  (==) (ETup  a) (ETup  b) = (a == b)
  (==) _         _         = False
