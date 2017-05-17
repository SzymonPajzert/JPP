{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -fdefer-typed-holes #-}

module Compile where

import Prelude hiding (exp)
import Data.List (partition)
import Control.Monad (liftM2, liftM3)

import qualified AbsGrammar as Abs
import ParGrammar
import LexGrammar
import qualified ErrM

import Err
import Exp

type ComErr = Err CompilationError
data CompilationError
  = NoMainDefinition
  | MultipleDefinitions Var
  | UnsupportedSyntaxExpr Abs.Exp
  | UnsupportedSyntaxDef  Abs.VDef
  | ParseError String
  deriving (Show)

type ParseFun a = [Token] -> ErrM.Err a
type Program = [Abs.TopDef]

parseGen :: ParseFun a -> String -> ErrM.Err a
parseGen p s = p $ myLexer s

parse :: String -> ComErr Program
parse fileCont = case parseGen pListTopDef fileCont of
  ErrM.Bad string -> Bad $ ParseError string
  ErrM.Ok program -> Ok program
  
desugar_prog :: Program -> ComErr Exp
desugar_prog program = do
    let vdefs = program >>= extract_vdef
    defs <- desugar_vdefs vdefs
    let 
    case partition (\(name, _) -> name == "main") defs of
      ([(_, main)], rest) -> return $ ELet rest main
      ([], _)             -> Bad $ NoMainDefinition
      _                   -> Bad $ MultipleDefinitions "main"   
    where
      extract_vdef (Abs.VarDef vdef) = [vdef]
      extract_vdef _                 = []

desugar_vdefs :: [Abs.VDef] -> ComErr [Def]
desugar_vdefs vdefs = sequence $ map desugar_vdef vdefs
    
desugar_vdef :: Abs.VDef -> ComErr Def
desugar_vdef def@(Abs.Def (Abs.Ident ident) binds exp) = lambda
  where
    lambda = do
      bound <- foldr add_bind (desugar exp) binds
      return $ (ident, bound)
    add_bind :: Abs.Bind -> ComErr Exp -> ComErr Exp
    add_bind (Abs.BVar (Abs.Ident bind_ident)) (Ok result) = Ok $ ELam result bind_ident
    add_bind _                                 err@(Bad _) = err
    add_bind _                                 _           = Bad $ UnsupportedSyntaxDef def

desugar :: (Abs.Exp) -> ComErr Exp
-- desugar (Abs.EMat _)
desugar (Abs.ELet vdefs exp) = do
  defs <- desugar_vdefs vdefs
  dexp <- desugar exp
  return $ ELet defs dexp
desugar (Abs.EIf cond true false) = (liftM3 EIf) (desugar cond) (desugar true) (desugar false)
desugar (Abs.ESmal left right) = (liftM2 $ EOp OpLes) (desugar left) (desugar right) 
desugar (Abs.EEq left right) = (liftM2 $ EOp OpEqu) (desugar left) (desugar right)
desugar (Abs.EAnd left right) = (liftM2 $ EOp OpAnd) (desugar left) (desugar right)
desugar (Abs.EOr left right) = (liftM2 $ EOp OpOr) (desugar left) (desugar right)

desugar (Abs.EAdd left right) = (liftM2 $ EOp OpAdd) (desugar left) (desugar right)
desugar (Abs.ESub left right) = (liftM2 $ EOp OpSub) (desugar left) (desugar right)
desugar (Abs.EMul left right) = (liftM2 $ EOp OpMul) (desugar left) (desugar right)
-- desugar (Abs.Cons _ _)
desugar (Abs.EApp func value) = (liftM2 EApp) (desugar func) (desugar value)
-- desugar (ELis _)
desugar (Abs.EInt int) = Ok $ EInt int
desugar (Abs.ECon (Abs.UIdent ident)) = Ok $ EVar ident
desugar (Abs.EVar (Abs.Ident ident)) = Ok $ EVar ident
desugar (Abs.ETup [exp]) = desugar exp
desugar (Abs.ETup exps) = ETup `fmap` (sequence (map desugar exps))

-- desugar unsupported = Bad $ UnsupportedSyntaxExpr unsupported
