

module AbsGrammar where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
newtype UIdent = UIdent String deriving (Eq, Ord, Show, Read)
newtype LIdent = LIdent String deriving (Eq, Ord, Show, Read)
data Program = Prog [TopDef]
  deriving (Eq, Ord, Show, Read)

data TopDef = VarDef VDef | TypDef TDef
  deriving (Eq, Ord, Show, Read)

data VDef = Def Ident [Bind] Exp
  deriving (Eq, Ord, Show, Read)

data Exp
    = EMat Exp [MCase]
    | ELet [VDef] Exp
    | EIf Exp Exp Exp
    | ELam [Ident] Exp
    | ESmal Exp Exp
    | EEq Exp Exp
    | EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | ECons Exp Exp
    | EApp Exp Exp
    | ELis [Exp]
    | ETup [Exp]
    | EInt Integer
    | ECon UIdent
    | EVar Ident
  deriving (Eq, Ord, Show, Read)

data MCase = MCas Bind Exp
  deriving (Eq, Ord, Show, Read)

data Bind
    = BLis [Bind]
    | BTup [Bind]
    | BVar Ident
    | BInt Integer
    | BSkip
    | BULis Bind Bind
    | BCon UIdent [Bind]
  deriving (Eq, Ord, Show, Read)

data TDef = TDef UIdent [LIdent] Type | TClar Ident Type
  deriving (Eq, Ord, Show, Read)

data Type
    = TBnb UIdent [Type]
    | TUnb LIdent
    | TTup [Type]
    | TLis Type
    | TFun Type Type
    | TCData [SubType]
  deriving (Eq, Ord, Show, Read)

data SubType = SubType UIdent [Type]
  deriving (Eq, Ord, Show, Read)

