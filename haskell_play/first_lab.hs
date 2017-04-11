-- TODO note - nice syntax (:) firstElem $ muchMoreComplicatedSecond

inits :: [a] -> [[a]]
inits [] = []
inits (x:xs) = [x] : (map (x:) (inits xs))

partitions :: [a] -> [([a], [a])]
partitions [] = [([], [])]
partitions (x:xs) = ([], (x:xs)) : map (\(ys, zs) -> (x:ys, zs)) (partitions xs)

perf_flatten :: ([b] -> a -> [b]) -> [a] -> [b]
perf_flatten _ [] = []
perf_flatten f (x:xs) = f (perf_flatten f xs) x 

halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs) 
  where len = length xs 
        half = len `div` 2

random_cross :: ([a], [a]) -> [[a]]
random_cross ([], rs) = [rs]
random_cross (ls, []) = [ls]
random_cross (l:ls, r:rs) = left ++ right 
  where left = map (l:) (random_cross (ls, r:rs))
        right = map (r:) (random_cross (l:ls, rs))

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations [x] = [[x]]
permutations xs = foldMap random_cross pairs
  where (left, right) = halve xs
        pairs = [(l, r) | l <- (permutations left), r <- (permutations right)]

-- TODO implement permutations as list comprehensions

-- NOTE alt-p goes to previous command in haskell interpreter in emacs

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : [y | y <- (nub xs), y /= x]

nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (x:xs) = x : (filter (x/=) $ nub xs)

-- Zaczynamy drugi lab
-- Haskell 01

triads :: Int -> [(Int,Int,Int)]
triads n = filter pyth $ three n
  where pyth (a, b, c) = a*a + b*b == c*c
        three n = [(c, b, a) | a <- [1..n], b <- [1..a], c <- [1..b]]

-- NOTE f $! a forces strict evaluation 

good_fact :: Integral a => a -> a
good_fact n = let
  fact acc 1 = acc
  fact acc n = (fact $! (acc * n)) (n-1)
  in fact 1 n

indexOf :: Char -> String -> Maybe Int
indexOf = let
  indexOfHelp _ _ [] = Nothing
  indexOfHelp num c (x:xs)
    | c == x = Just num
    | otherwise = indexOfHelp (num+1) c xs 
  in indexOfHelp 0

positions :: Char -> String -> [Int]
positions c s = foldr f [] zipped
  where f (char, index) acc = (if char == c then (index :) else (\x -> x)) acc
        zipped = zip s [0..]


{-
important difference between the two types of binding
is that the monadic bind (p <- e) is strict (it evaluates e),
whereas with the let form, the expression isn't evaluated immediately
-}

-- Kompilacja programów

-- z użyciem interact
-- smain :: String -> String
-- main = interact smain

-- lub, dla prostszych programów, z print:
-- mymain :: String 
-- main = putStrLn mymain

-- runhaskell hello.hs - odpal kod haskella

-- ghc -o hello --make hello.hs - kompilacja kodu

-- TODO Nauka cabala

-- Warto pamiętać:
-- concat :: Foldable t => t [a] -> [a]
-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]

-- mutatis mutandis
-- used when comparing two or more cases or situations
-- making necessary alterations while not affecting the main point at issue.

-- it zapisuje w ghci wynik poprzedniego polecenia

-- TODO check difference between strict and lazy map
-- TODO Data.Map and Data.Set

-- TODO read http://dev.stephendiehl.com/hask/tutorial.pdf
-- Wygląda bardzo ciekawie
