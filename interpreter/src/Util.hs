module Util where

import Prelude hiding (unlines, exp)
import Control.Monad.Reader
import Exp


type IndentString = Reader Int String

-- TODO probably remove
-- | Return two lists only if they are matching length
safeZip :: [a] -> [b] -> Maybe [(a, b)]
safeZip (a:as) (b:bs) = fmap ((:) (a, b)) $ safeZip as bs
safeZip []     []     = Just []
safeZip _      _      = Nothing

-- | Print expression in indented way
indent :: Exp -> String
indent exp = runReader (print_indent exp) 0

-- | Create indented version of string
make_indent :: String -> IndentString
make_indent word = do
  n <- ask
  return $ (take n $ repeat ' ') ++ word

-- | Out version of unlines which doesn't append newline to last line
unlines :: [String] -> String
unlines [] = []
unlines [line] = line
unlines (l:ls) = l ++ ('\n' : (unlines ls))

-- | Create indented version of exp
print_indent :: Exp -> IndentString
print_indent (EApp fun arg) = do
  funS <- print_indent fun
  argS <- local (+2) (print_indent arg)
  return $ unlines [funS,argS]

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

print_indent (ETup tuple) = do
  make_indent (unwords (map show tuple))

print_indent (EMat exp binds) = do
  matchS <- make_indent "match"
  expS <- local (+2) (print_indent exp)
  withS <-  make_indent "with"
  bindsS <- mapM (\bind -> local (+2) (print_indent_bind bind)) binds

  return $ unlines $ [matchS, expS, withS] ++ bindsS
  where
    print_indent_bind (bind, expBind) = do
      bindS <- make_indent $ (show bind) ++ " ->"
      expS <- local (+2) (print_indent expBind)
      return $ unlines [bindS, expS]

print_indent (ECon name params) = do
  nameS <- make_indent name
  paramsS <- sequence $ map print_indent params

  return $ unlines (nameS : paramsS)
