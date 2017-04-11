import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Monad.State

-- Funkcja daje listę wszystkich list dwuelementowych, gdzie pierwszy element należy do pierwszego argumentu, drugi do drugiego, np.
allPairs1 :: [a] -> [a] -> [[a]]
allPairs1 xs ys = [[x,y] | x <- xs, y <- ys]

-- allPairs [1,2,3] [4,5] == [[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]
-- przepisz na notację monadyczną
allPairs :: [a] -> [a] -> [[a]]
allPairs xs ys = do
  x <- xs
  y <- ys
  return [x, y]
  
-- uogólnij te funkcję do allCombinations :: [[a]] -> [[a]] , która dla n-elementowej listy list da listę wszystkich n-elementowych list takich, że i-ty element należy do i-tego elementu argumentu, np
-- allCombinations [[1,2], [4,5], [6], [7]] == [[1,4,6,7],[1,5,6,7],[2,4,6,7],[2,5,6,7]]
allCombinations :: [[a]] -> [[a]]
allCombinations [xs] = map (:[]) xs
allCombinations xss = do
  let xs:yss = xss
  x <- xs
  comb <- allCombinations yss
  return $ x:comb

-- Zadanie 2. Reader
data Tree a
  = Empty
  | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

--Napisz funkcję która dla danego drzewa da drzewo, gdzie w każdym węźle przechowywana będzie głębokość tego węzła
renumber :: Tree a -> Tree Int
renumber = renumberHelp 0
  where
    renumberHelp :: Int -> Tree a -> Tree Int
    renumberHelp n Empty = Node n Empty Empty
    renumberHelp n (Node _ left right) = Node n (renumberHelp (n+1) left) (renumberHelp (n+1) right)


-- NOTE można użyć fromMaybe (error "no value") (M.lookup map v)

-- TODO ^ Porównaj rozwiązania z użyciem monady Reader i bez.
-- a. zaprojektuj składnię konkretną Sugestie: standardowa notacja infiksowa oraz notacja prefiksowa a la Lisp: (* (+ 1 2) 3)

type Var = String
data Exp
  = EInt Int
  | EOp Op Exp Exp
  | EVar Var
  | ELet Var Exp Exp

data Op
  = OpAdd
  | OpMul
  | OpSub

evalOp :: Op -> (Int -> Int -> Int)
evalOp OpAdd = (+)
evalOp OpMul = (*)
evalOp OpSub = (-)

type Bindings = Map.Map Var Int

lookupVar :: Var -> Bindings -> Int
lookupVar name map = map Map.! name

addBinding :: Var -> Int -> Bindings -> Bindings
addBinding = Map.insert

-- Napisz funkcję ktora obliczy wartość takiego wyrażenia, np
helpEvalExp :: Exp -> Reader Bindings Int
helpEvalExp (EInt n) = return n
helpEvalExp (EOp op left right) = do
  left <- evalExp left
  right <- evalExp right
  return $ evalOp op left right
helpEvalExp (EVar name) = do
  asks (lookupVar name)
helpEvalExp (ELet var assign next) = do
  evalAssigned <- evalExp assign
  local (addBinding var evalAssigned) (evalExp next)

evalExp :: Bindings -> Exp -> Int
evalExp bind exp = runReader (helpEvalExp exp) bind
{-
let var = e1 in e2

let x =
      let y = 6 + 9
      in y - 1
  in x * 3
-}


test = ELet "x"
  (ELet "y" (EOp OpAdd (EInt 6) (EInt 9))
    (EOp OpSub y (EInt 1)))
  (EOp OpMul x (EInt 3))
  where x = EVar "x"
        y = EVar "y"

result = evalExp (Map.empty) (evalExp test)

--(można też uzyć typu wyrażeń z jednego z poprzednich labów)
-- Użyj monady czytelnika środowiska (Reader). Środowisko może być np. jednego z typów
-- Map Var Int
-- [(Var, Int)]
-- Var -> Maybe Int


{-

-- Zadanie 3. Monada State
-- a. Dany typ drzew
-- data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)
Napisz funkcję która ponumeruje wezly drzewa tak, ze kazdy z nich bedzie mial inny numer. Porownaj rozwiazania z
uzyciem monady State i bez.

możliwe dodatkowe wymaganie: ponumeruj wezly drzewa w kolejnosci infiksowej.
-}

{- TODO
renumberTree :: Tree a -> Tree Int
renumberTree Empty = Empty



(toList $ renumber $ fromList "Learn Haskell") == [0..12]
-}

{-b. Rozszerzmy język z poprzedniego zadania o instrukcje języka Tiny (patrz przedmiot Semantyka i
Weryfikacja Programów)
Stmt:
S ::= skip | x := e | S1;S2
| if b then S1 else S2 | while b do S
-}

data Stmt
  = SSkip
  | SAssign Var Exp
  | SSeq Stmt Stmt
  | SIf Exp Stmt Stmt
  | SWhile Exp Stmt
--  SOutput Expr

helpExecStmt :: Stmt -> State () Bindings
helpExecStmt SSkip = return ()
helpExecStmt (SAssign var expr) = do
  modify (addBinding var expr)
helpExecStmt (SSeq first second) = do
  helpExecStmt first
  helpExecStmt second
helpExecStmt (SIf expr true false) = do
  value <- gets $ (evalExp expr)
  helpExecStmt $ if value != 0 then true else false
helpExecStmt (SWhile expr stmt) = do
  value <- gets $ (evalExp expr)
  helpExecStmt $ if value != 0 then stmt else SSkip


-- korzystając z wcześniej napisanej funkcji evalExp , napisz funkcję
-- która wykona podaną instrukcję (program) i wypisze stan końcowy (w tym wypadku wartości zmiennych)
execStmt :: Stmt -> IO ()
execStmt stmt = do
  let finalState = execState stmt (Map.empty)  
  putStrLn $ show $ finalState 
{-
c. Następny krok to dodanie deklaracji zmiennych lokalnych:
Stmt: S ::= ... | begin [D] S end
Decl: D ::= var x=e;
Tutaj dla obliczania Stmt najlepiej użyć jednocześnie monady Reader i State: część Reader odczytuje
funkcje z Var w nowy typ "lokacji pamięci" Loc(=Int), a część State operuje na funkcjach z Loc w Int,
implementowanych jako mapy. Trzeba też zaimplementować funkcję alloc :: (Map Loc Int) -> Loc która
zwraca nieużywaną lokację.
Zadanie 4. Transformatory monad
a. Zaimplementuj moduły StateTParser i SimpleParsers z wykładu
b. Zaimplementuj moduł IdentityTrans dostarczający identycznościowego transformatora IdentityT:
newtype IdentityT = ...
instance (Functor m) => Functor (IdentityT m) where
instance (Monad m) => Monad (IdentityT m) ...
instance MonadTrans IdentityT ...
instance MonadPlus m => MonadPlus (IdentityT m) ...
c. Zaimplementuj moduł MaybeTrans dostarczający transformatora MaybeT - analogicznie jak wyżej, za
wyjatkiem
instance MonadPlus (MaybeT m) ...
d. Zaimplementuj moduł StateTParser2 z wykładu:
module StateTParser2(Parser,runParser,item) where
import Control.Monad.State
import MaybeTrans
import Control.Monad.Identity
-- Use the StateT transformer on MaybeT on Identity
type Parser a = StateT [Char] (MaybeT Identity) a
runParser :: Parser a -> [Char] -> Maybe (a,String)
item :: Parser Char
i przetestuj go z SimpleParsers (zastępując import StateTParser przez import StateTParser2)module SimpleParsers where
import Data.Char(isDigit,digitToInt)
import Control.Monad
import StateTParser2
pDigit1 :: Parser Int
pDigit1 = item >>= test where
test x | isDigit x = return $ digitToInt x
| otherwise = mzero
sat :: (Char->Bool) -> Parser Char
sat p = do {x <- item; if p x then return x else mzero}
char :: Char -> Parser Char
char x = sat (==x)
pDigit :: Parser Int
pDigit = fmap digitToInt $ sat isDigit
pDigits :: Parser [Int]
pDigits = many pDigit
pNat :: Parser Int
-- pNat = pDigits >>= return . foldl (\x y -> 10*x+y) 0
pNat = fmap (foldl (\x y -> 10*x+y) 0) pDigits
many :: Parser a -> Parser [a]
many p = many1 p `mplus` return []
many1 p = do { a <- p; as <- many p; return (a:as)}
test123 = runParser pNat "123 ala"
-}
