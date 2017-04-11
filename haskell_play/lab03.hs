import Prelude hiding (Applicative, sequence, mapM, pure, (<*>), (*>), (<*))
{-
TODO Learn about Functor => Pointed in documentation
https://wiki.haskell.org/Why_not_Pointed%3F
http://learnyouahaskell.com/functors-applicative-functors-and-monoids
TODO Zobaczyć różnice między Applicative a Functor
Pointed pure law: fmap f (pure a) = pure (f x)

mapM :: (a -> m b) -> [a] -> m [b]
które dla monady either pozwala na fajne parsowanie

TODO Użyć to do parsowania listy

words input = groupBy ((==) `on` isSpace) $ input
Typical usage: sortBy (compare `on` fst).

Tworzenie DSL w haskellu

a + b :: Exp
za pomocą
-}


{-
instance Num Exp where
  a + b = EAdd a b
  a * b = EMul a b
  a - b = ESub a b
  negate a = Esub (EInt 0) a
  -- etc
-}

{-Zadanie 3.
Uzupełnić przykład z wykładu:-}

{-
import Control.Monad.Error

data ParseError = Err {location::Int, reason::String}
instance Error ParseError where
  
type ParseMonad = Either ParseError
parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHex :: String -> ParseMonad Integer
toString :: Integer -> ParseMonad String

-- convert zamienia napis z liczba szesnastkowa 
--   na napis z liczba dziesietna
convert :: String -> String
convert s = str where
 (Right str) = tryParse s `catchError` printError
 tryParse s = do {n <- parseHex s; toString n}
 printError e = ...
  -}

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (xm:xms) = do
  x <- xm
  xs <- sequence xms
  return $ x:xs

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f 
  
forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM

{-Zadanie 5. (opcjonalne)
Nieco inny od monad model obliczeń reprezentuje klasa Applicative-}

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f(a->b) -> f a -> f b 

{-pure to to samo co return
Operator (<*>) reprezentuje sekwencjonowanie obliczeń podobne do (=<<) z tym, że kolejne obliczenie nie zależy od wyniku poprzedniego (choć jego wynik oczywiście może).
Dla każdej monady można zdefiniować instancję Applicative:

pure = return
mf <*> ma =  do { f <- mf; a <- ma; return mf ma }

-}
-- a. Zdefiniuj instancje Applicative dla Maybe i Either b.

instance Applicative Monad where
  pure = Just
  Just f <*> Just x = Just $ f x
  _ <*> _ = Nothing

--Zdefiniuj operację *> będącą analogiem >> (czyli ignorującą wynik pierwszego obliczenia):

(*>) :: Applicative f => f a -> f b -> f b
a *> b = pure (\b -> const b) <*> b <*> a 

(<*) :: Applicative f => f a -> f b -> f a
a <* b = flip (*>) a b
