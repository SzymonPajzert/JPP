-- TOOD Zadanie 4
-- Monada to obliczenie wyniku, chociaż raczej obliczanie.

-- TODO Notatka Zadania się zmienią z Prologu i Smalltalka

import Prelude (Int, String, id, Num)

data Exp 
  = EInt Int             -- stała całkowita       
  | EAdd Exp Exp         -- e1 + e2
  | ESub Exp Exp         -- e1 - e2
  | EMul Exp Exp         -- e1 * e2
  | EVar String          -- zmienna
  | ELet String Exp Exp  -- let var = e1 in e2

class ImplicitExp a where
  get :: a -> Exp

instance ImplicitExp Int where
  get int = EInt int

instance ImplicitExp Exp where
  get = id

(+) :: (ImplicitExp a, ImplicitExp b) => a -> b -> Exp
l + r = EAdd (get l) (get r)

(*) :: (ImplicitExp a, ImplicitExp b) => a -> b -> Exp
l * r = EMul (get l) (get r)

expr :: Exp
expr = (2 + 2) * 3

-- TODO Zadanie 6
