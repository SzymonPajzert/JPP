{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -fdefer-typed-holes #-}

module Compile where

import Prelude hiding (exp, head, tail)
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

desugarVdef (Abs.VPat bind exp) = do
  bitterBind <- desugarBind bind
  bitterExp <- desugar exp
  let triples = (\ident -> (ident, bitterBind, bitterExp)) `map` extractVars bind 
  Ok $ map desugarVariableBind triples
  
desugarVariableBind :: (Var, Bind, Exp) -> Def    
desugarVariableBind (var, bind, exp) = (var, EMat exp [(bind, EVar var)])

desugarBind :: Abs.Bind -> ComErr Bind
desugarBind bind = if uniqueIdents bind then case bind of
  Abs.BSkip                  -> Ok BIgnore
  Abs.BVar (Abs.Ident ident) -> Ok $ BVar ident
  Abs.BTup [ident]           -> desugarBind ident
  Abs.BTup idents            -> do
    newIdents <- sequence $ map desugarBind idents
    return $ BTup newIdents
  Abs.BLis binds             -> do
    newBinds <- sequence $ map desugarBind binds
    return $ createBitterList newBinds
  Abs.BULis leftB rightB     -> do
    left  <- desugarBind leftB
    right <- desugarBind rightB
    return $ BPol listCons [left, right]
  Abs.BCon (Abs.UIdent ident) binds -> do
    newBinds <- sequence $ map desugarBind binds
    return $ BPol ident newBinds
  Abs.BInt int -> return $ BInt int 
    
  else Bad MultipleDefinitionsBinds

createBitterList :: [Bind] -> Bind
createBitterList binds = foldr addBind nil binds
  where
    nil               = BPol emptyList []
    addBind bind tail = BPol listCons  [bind, tail]


uniqueIdents :: Abs.Bind -> Bool
uniqueIdents bind = unique $ extractVars bind
    where unique list = (nub list) == list

extractVars :: Abs.Bind -> [Var]
extractVars anyBind = case anyBind of
  Abs.BLis binds         -> binds >>= extractVars
  Abs.BTup binds         -> binds >>= extractVars
  Abs.BCon _ binds       -> binds >>= extractVars

  Abs.BULis head tail    -> [head,tail] >>= extractVars

  Abs.BVar (Abs.Ident ident) -> [ident]
  Abs.BInt _             -> []
  Abs.BSkip              -> []


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
desugar (Abs.ELis listExp) = do
  bitterList <- mapM desugar listExp
  return $ foldr addElementToList (ECon emptyList []) bitterList
  where
    addElementToList element list = ECon listCons [element, list]
  
desugar (Abs.EInt int) = Ok $ EInt int
desugar (Abs.ECon (Abs.UIdent ident)) = Ok $ EVar ident
desugar (Abs.EVar (Abs.Ident ident)) = Ok $ EVar ident
desugar (Abs.ETup [exp]) = desugar exp
desugar (Abs.ETup exps) = ETup `fmap` (sequence (map desugar exps))
desugar (Abs.ELam idents exp) = desugarLambda exp idents
desugar (Abs.EMat expS mcases) = do
  exp <- desugar expS

  let desugarCase (Abs.MCas bindS contS) = do
        bind <- desugarBind bindS
        cont <- desugar contS
        return (bind, cont)
  
  newCases <- sequence $ map desugarCase mcases

  return $ EMat exp newCases
desugar (Abs.ECons headS tailS) = do
  head <- desugar headS
  tail <- desugar tailS

  return $ ECon listCons [head, tail]

desugarLambda :: Abs.Exp -> [Abs.Bind] -> ComErr Exp
desugarLambda expS identsS = do
  exp <- desugar expS
  idents <- sequence $ map desugarBind identsS

  let lambda = foldr (\ident expAcc -> ELam expAcc ident) exp idents

  return lambda
  
  

-- desugar unsupported = Bad $ UnsupportedSyntaxExpr unsupported
