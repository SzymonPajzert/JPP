module Util where

import Prelude hiding (unlines, exp)
import Control.Monad.Reader
import Exp

indent :: Exp -> String
indent exp = runReader (print_indent exp) 0

make_indent :: String -> Reader Int String
make_indent word = do
  n <- ask
  return $ (take n $ repeat ' ') ++ word 

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
  letS <- make_indent "let"
  defsS <- mapM print_def defs
  inS <- make_indent "in"
  expS <- local (+2) $ print_indent finalExp
  return $ unlines $ [letS] ++ defsS ++ [inS, expS]
    where
      print_def :: Def -> Reader Int String
      print_def (def, value) = do
        defS <- local (+2) (make_indent (def ++ " ="))
        expS <- local (+4) (print_indent value)
        return $ unlines [defS, expS]

print_indent (ELam exp var) = do
  varS <- make_indent (show var)
  expS <- local (+2) (print_indent exp)
  return $ unlines $ [varS, expS]

print_indent (EVar var) = do
  varS <- make_indent var
  return $ unlines [varS]

print_indent (EOp op exp1 exp2) = do
  opS <- make_indent (show op)
  exp1S <- local (+2) (print_indent exp1)
  exp2S <- local (+2) (print_indent exp2)
  return $ unlines [opS, exp1S, exp2S]

print_indent (EInt value) = do
  make_indent $ show value

print_indent (EBool bool) = do
  make_indent $ show bool

print_indent (ETup tuple) = do
  make_indent (unwords (map show tuple)) 

print_indent (EMat exp binds) = do
  matchS <- make_indent "match"
  expS <- local (+2) (print_indent exp)
  withS <-  make_indent "with"
  bindsS <- mapM (\bind -> local (+2) (print_indent_bind bind)) binds

  return $ unlines $ [matchS, expS, withS] ++ bindsS 

print_indent (ECon name params) = do
  nameS <- make_indent name
  paramsS <- sequence $ map print_indent params

  return $ unlines (nameS : paramsS)

print_indent_bind (bind, exp) = do
  bindS <- make_indent $ (show bind) ++ " ->"
  expS <- local (+2) (print_indent exp)
  return $ unlines [bindS, expS]
