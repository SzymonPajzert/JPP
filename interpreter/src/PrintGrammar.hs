{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintGrammar where

-- pretty-printer generated by the BNF converter

import AbsGrammar
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print UIdent where
  prt _ (UIdent i) = doc (showString ( i))


instance Print LIdent where
  prt _ (LIdent i) = doc (showString ( i))
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])


instance Print Program where
  prt i e = case e of
    Prog topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print TopDef where
  prt i e = case e of
    VarDef vdef -> prPrec i 0 (concatD [prt 0 vdef])
    TypDef tdef -> prPrec i 0 (concatD [prt 0 tdef])
  prtList _ [x] = (concatD [prt 0 x, doc (showString ";")])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print VDef where
  prt i e = case e of
    Def id binds exp -> prPrec i 0 (concatD [prt 0 id, prt 1 binds, doc (showString "="), prt 0 exp])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print Exp where
  prt i e = case e of
    EMat exp mcases -> prPrec i 0 (concatD [doc (showString "match"), prt 0 exp, doc (showString "with"), doc (showString "{"), prt 0 mcases, doc (showString "}")])
    ELet vdefs exp -> prPrec i 0 (concatD [doc (showString "let"), doc (showString "{"), prt 0 vdefs, doc (showString "}"), prt 0 exp])
    EIf exp1 exp2 exp3 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 exp1, doc (showString "then"), prt 0 exp2, doc (showString "else"), prt 0 exp3])
    ELam ids exp -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 ids, doc (showString "->"), prt 0 exp])
    ESmal exp1 exp2 -> prPrec i 0 (concatD [prt 1 exp1, doc (showString "<"), prt 1 exp2])
    EEq exp1 exp2 -> prPrec i 0 (concatD [prt 1 exp1, doc (showString "="), prt 1 exp2])
    EAdd exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "+"), prt 2 exp2])
    ESub exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "-"), prt 2 exp2])
    EMul exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "*"), prt 3 exp2])
    EDiv exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "/"), prt 3 exp2])
    ECons exp1 exp2 -> prPrec i 3 (concatD [prt 4 exp1, doc (showString "::"), prt 3 exp2])
    EApp exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, prt 5 exp2])
    ELis exps -> prPrec i 5 (concatD [doc (showString "["), prt 0 exps, doc (showString "]")])
    ETup exps -> prPrec i 5 (concatD [doc (showString "("), prt 0 exps, doc (showString ")")])
    EInt n -> prPrec i 5 (concatD [prt 0 n])
    ECon uident -> prPrec i 5 (concatD [prt 0 uident])
    EVar id -> prPrec i 5 (concatD [prt 0 id])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print MCase where
  prt i e = case e of
    MCas bind exp -> prPrec i 0 (concatD [doc (showString "|"), prt 0 bind, doc (showString "->"), prt 0 exp])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Bind where
  prt i e = case e of
    BLis binds -> prPrec i 1 (concatD [doc (showString "["), prt 0 binds, doc (showString "]")])
    BTup binds -> prPrec i 1 (concatD [doc (showString "("), prt 0 binds, doc (showString ")")])
    BVar id -> prPrec i 1 (concatD [prt 0 id])
    BInt n -> prPrec i 1 (concatD [prt 0 n])
    BSkip -> prPrec i 1 (concatD [doc (showString "_")])
    BULis bind1 bind2 -> prPrec i 0 (concatD [prt 1 bind1, doc (showString "::"), prt 1 bind2])
    BCon uident binds -> prPrec i 0 (concatD [prt 0 uident, prt 1 binds])
  prtList 1 [] = (concatD [])
  prtList 1 (x:xs) = (concatD [prt 1 x, prt 1 xs])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print TDef where
  prt i e = case e of
    TDef uident lidents type_ -> prPrec i 0 (concatD [prt 0 uident, prt 0 lidents, doc (showString "="), prt 0 type_])
    TClar id type_ -> prPrec i 0 (concatD [prt 0 id, doc (showString ":"), prt 0 type_])

instance Print Type where
  prt i e = case e of
    TBnb uident types -> prPrec i 3 (concatD [prt 0 uident, prt 3 types])
    TUnb lident -> prPrec i 3 (concatD [prt 0 lident])
    TTup types -> prPrec i 3 (concatD [doc (showString "("), prt 0 types, doc (showString ")")])
    TLis type_ -> prPrec i 3 (concatD [doc (showString "["), prt 2 type_, doc (showString "]")])
    TFun type_1 type_2 -> prPrec i 2 (concatD [prt 2 type_1, doc (showString "->"), prt 3 type_2])
    TCData subtypes -> prPrec i 1 (concatD [doc (showString "adt"), doc (showString "{"), prt 0 subtypes, doc (showString "}")])
  prtList 3 [] = (concatD [])
  prtList 3 (x:xs) = (concatD [prt 3 x, prt 3 xs])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print SubType where
  prt i e = case e of
    SubType uident types -> prPrec i 0 (concatD [doc (showString "|"), prt 0 uident, prt 3 types])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

