{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -fdefer-typed-holes #-}

module Compile where

import Prelude hiding (exp)
import Data.List (partition, nub)
import Control.Monad (liftM2, liftM3)

import qualified AbsGrammar as Abs
import ParGrammar
import LexGrammar
import qualified ErrM (Err(..))

import Err
import Exp

type ComErr = Err CompilationError
data CompilationError
  = NoMainDefinition
  | MultipleDefinitions Var
  | ParseError String
  | MultipleDefinitionsBinds
  deriving (Show)

type ParseFun a = [Token] -> ErrM.Err a
type Program = [Abs.TopDef]

parseGen :: ParseFun a -> String -> ErrM.Err a
parseGen p s = p $ myLexer s

parse :: String -> ComErr Program
parse fileCont = case parseGen pListTopDef fileCont of
  ErrM.Bad string -> Bad $ ParseError string
  ErrM.Ok program -> Ok program
  
desugarProg :: Program -> ComErr Exp
desugarProg program = do
    let vdefs = program >>= extract_vdef
    defs <- desugarVdefs vdefs
    let 
    case partition (\(name, _) -> name == "main") defs of
      ([(_, main)], rest) -> return $ ELet rest main
      ([], _)             -> Bad $ NoMainDefinition
      _                   -> Bad $ MultipleDefinitions "main"   
    where
      extract_vdef (Abs.VarDef vdef) = [vdef]
      extract_vdef _                 = []

desugarVdefs :: [Abs.VDef] -> ComErr [Def]
desugarVdefs vdefs = do
  toFlat <- sequence $ (map desugarVdef vdefs)
  return $ concat toFlat
    
desugarVdef :: Abs.VDef -> ComErr [Def]
desugarVdef (Abs.VDef (Abs.Ident ident) binds exp) = lambda
  where
    lambda = do
      bound <- foldr add_bind (desugar exp) binds
      return $ [(ident, bound)]
    add_bind :: Abs.Bind -> ComErr Exp -> ComErr Exp
    add_bind bind (Ok result) = do
      bitterBind <- desugarBind bind
      Ok $ ELam result bitterBind
    add_bind _    err@(Bad _) = err

{-
desugarVdef (Abs.VPat bind exp) = do
  bitterBind <- desugar bind
  case bitterBind of
-}

desugarBind :: Abs.Bind -> ComErr Bind
desugarBind bind = if uniqueIdents bind then case bind of
  Abs.BVar (Abs.Ident ident) -> Ok $ BVar ident
  Abs.BTup [ident]           -> desugarBind ident
  Abs.BTup idents  -> do
    newIdents <- sequence $ map desugarBind idents
    return $ BTup newIdents
  else Bad MultipleDefinitionsBinds

uniqueIdents :: Abs.Bind -> Bool
uniqueIdents bind = unique $ extractVars bind
  where
    extractVars anyBind = case anyBind of
      Abs.BLis binds         -> binds >>= extractVars
      Abs.BTup binds         -> binds >>= extractVars
      Abs.BCon _ binds       -> binds >>= extractVars

      Abs.BULis head tail    -> [head,tail] >>= extractVars

      Abs.BVar (Abs.Ident ident) -> [ident]
      Abs.BInt _             -> []
      Abs.BSkip              -> []

    unique list = (nub list) == list



desugar :: (Abs.Exp) -> ComErr Exp
-- desugar (Abs.EMat _)
desugar (Abs.ELet vdefs exp) = do
  defs <- desugarVdefs vdefs
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
