module Exp where

import Prelude (Show, Eq, show, String, Integer, Bool(..), (==))

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

data DynVal
  = TInt Integer
  | TBool Bool
  | TTuple [DynVal]
--  identyfikatory typów polimorficznych (listy i booleany będą tutaj)
--deriving (Show, Eq)

instance Show DynVal where
  show (TInt int) = show int
  show (TBool bool) = show bool

instance Eq DynVal where
  (==) (TInt  a) (TInt  b) = (a == b)
  (==) (TBool a) (TBool b) = (a == b)
  (==) _         _         = False

data Exp
  = EApp Exp Exp
  | EIf Exp Exp Exp
  | ELet [Def] Exp
  | ELam Exp Var
  | EOp Op Exp Exp
  | EVar Var
  | EVal DynVal
  deriving (Show)

