{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -fdefer-typed-holes #-}

module Compile (parse, desugarProg) where

import Prelude hiding (exp, head, tail)
import Data.List (partition, nub)
import Data.Function (on)
import qualified Data.Map as Map
import Control.Monad (liftM2, liftM3)

import qualified AbsGrammar as Abs
import ParGrammar
import LexGrammar
import qualified ErrM (Err(..))

import Err
import Exp hiding (Environment, bindings, emptyEnv)
import qualified Exp as Exp

type ComErr = Err CompilationError
data CompilationError
  = NoMainDefinition
  | MultipleDefinitions Var
  | ParseError String
  | MultipleDefinitionsBinds
  | UnsupportedType Abs.TDef -- TODO remove
  | EmptyDataDefinition
  deriving (Show)

type ParseFun a = [Token] -> ErrM.Err a
type Program = [Abs.TopDef]

parseGen :: ParseFun a -> String -> ErrM.Err a
parseGen p s = p $ myLexer s

parse :: String -> ComErr Program
parse fileCont = case parseGen pListTopDef fileCont of
  ErrM.Bad string -> Bad $ ParseError string
  ErrM.Ok program -> Ok program

desugarProg :: Program -> ComErr CompiledProgram
desugarProg program = do
    let vdefs = program >>= extract_vdef
    let tdefs = program >>= extract_tdef

    newEnvironment <- parseEnvironment tdefs
    defs <- desugarVdefs vdefs

    case partition (\(name, _) -> name == "main") defs of
      ([(_, main)], rest) -> return $ (ELet rest main, peel newEnvironment)
      ([], _)             -> Bad $ NoMainDefinition
      _                   -> Bad $ MultipleDefinitions "main"
    where
      extract_vdef (Abs.VarDef vdef) = [vdef]
      extract_vdef _                 = []

      extract_tdef (Abs.TypDef (tdef@(Abs.TDef _ _ _))) = [tdef]
      extract_tdef _                              = []

desugarVdefs :: [Abs.VDef] -> ComErr [Def]
desugarVdefs vdefs = do
  toFlat <- sequence $ (map desugarVdef vdefs)
  return $ concat toFlat

desugarVdef :: Abs.VDef -> ComErr [Def]
desugarVdef (Abs.VDef (Abs.LIdent ident) binds exp) = lambda
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
  Abs.BVar (Abs.LIdent ident) -> Ok $ BVar ident
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

  Abs.BVar (Abs.LIdent ident) -> [ident]
  Abs.BInt _             -> []
  Abs.BSkip              -> []


desugar :: (Abs.Exp) -> ComErr Exp
desugar (Abs.ELet vdefs exp) = do
  defs <- desugarVdefs vdefs
  dexp <- desugar exp
  return $ ELet defs dexp

desugar (Abs.EIf condSweet trueSweet falseSweet) = do
  cond <- desugar condSweet
  trueExp <- desugar trueSweet
  falseExp <- desugar falseSweet

  return $ EMat cond [(BPol truePol [], trueExp), (BPol falsePol [], falseExp)]


desugar (Abs.ESmal left right) = (liftM2 $ EOp OpLes) (desugar left) (desugar right) 
desugar (Abs.EEq left right) = (liftM2 $ EOp OpEqu) (desugar left) (desugar right)
desugar (Abs.EAnd leftSweet rightSweet) = do
  left <- desugar leftSweet
  right <- desugar rightSweet
  return $ (EMat left [(BPol truePol [], right), (BPol falsePol [], false)])

desugar (Abs.EOr leftSweet rightSweet) = do
  left <- desugar leftSweet
  right <- desugar rightSweet
  return $ (EMat left [(BPol truePol [], true), (BPol falsePol [], right)])

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
desugar (Abs.EVar (Abs.LIdent ident)) = Ok $ EVar ident
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


{-                      Helpers                         -}
-- | lazy stream of protected unbound identifiers
identifiers :: [Var]
identifiers = map (uncurry (++)) $ repeat "_unbound_x_" `zip` (map show ([1..] :: [Int]))


{-                   TYPES et cetera                    -}

type TypeClar = (Var, Type)

data Type =
  Solid String

-- | Creation of type instance for given identifier and type arguments
-- | create instance of that type
type TypeInst = (Var, [Abs.Type], Type)

data ComEnvir = ComEnvir
  { bindings :: Bindings
  , types :: [TypeClar] }

emptyEnv :: ComEnvir
emptyEnv = ComEnvir startBindings []
  where startBindings = Bindings $ Map.fromList [
          ("True", (true, Exp.emptyEnv)),
          ("False", (false, Exp.emptyEnv))
          ]

-- | Peels compilation environment to suitable runtime Environment
peel :: ComEnvir -> Exp.Environment
peel comEnv = Exp.Environment (bindings comEnv)

put :: Exp.Environment -> ComEnvir
put environment = ComEnvir (Exp.bindings environment) []

-- | Merge two compilation environments
-- | If there exist contadictions, return appropriate compilation error
safeMergeEnv :: ComErr ComEnvir -> ComErr ComEnvir -> ComErr ComEnvir
safeMergeEnv leftM rightM = do
  left <- leftM
  right <- rightM

  _ <- (checkContradictingBinds `on` bindings) left right
  return $ ComEnvir ((mergeBindings `on` bindings) left right) []

checkContradictingBinds :: Bindings -> Bindings -> ComErr ()
checkContradictingBinds bind1 bind2 = Ok () -- TODO implement

-- | Parses type definitions to create compilation environment
parseEnvironment :: [Abs.TDef] -> ComErr ComEnvir
parseEnvironment tdef = foldl safeMergeEnv (Ok emptyEnv) (map getInformation tdef)

-- | Creates small compilation environment for
getInformation :: Abs.TDef -> ComErr ComEnvir
getInformation (Abs.TDef (Abs.UIdent ident) typeParams newType) =
  case newType of
    Abs.TCData [] -> Bad $ EmptyDataDefinition
    Abs.TCData subtypes -> do
      let mapSubtype (Abs.SubType (Abs.UIdent uident) params) =
            defineConsFunction (uident, params, Solid ident)
      subtypesEnvirons <- mapM mapSubtype subtypes
      foldl safeMergeEnv (Ok emptyEnv) $ map return subtypesEnvirons

getInformation tdef = Bad $ UnsupportedType tdef


-- | Defines function and adds it to the environment
-- for example
-- > Node Int Tree Tree
-- Defines function taking three arguments and returning
defineConsFunction :: TypeInst -> ComErr ComEnvir
defineConsFunction (uident, params, returnType) = do
  let param_len = length params
  let idents = take param_len identifiers
  let finalExp = ECon uident (map EVar idents)

  let constructor = foldr addIdent finalExp idents

  return $ put $ addBinding uident (constructor, Exp.emptyEnv) Exp.emptyEnv

  where addIdent ident exp = ELam exp (BVar ident)
