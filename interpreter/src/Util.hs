module Util where

import Prelude hiding (unlines)
import Control.Monad.Reader
import Exp

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
