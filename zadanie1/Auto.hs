module Auto (Auto, accepts, emptyA, epsA, symA, leftA, sumA, thenA, fromLists, toLists) where
import Data.List


data Auto a q = A { states      :: [q]
                  , initStates  :: [q]
                  , isAccepting :: q -> Bool
                  , transition  :: q -> a -> [q]
                  }

areAccepting :: Auto a q -> [q] -> Bool
areAccepting automata states = foldl (||) False (map checker states)
  where checker = isAccepting automata

accepts :: Eq q => Auto a q -> [a] -> Bool
accepts automata [] = areAccepting automata $ initStates automata
accepts automata (char:chars) = newAutomata `accepts` chars
  where newAutomata = automata { initStates = newInit }
        newInit = foldr iter [] (initStates automata)
        iter state newStates = newStates `union` (transition automata state char) 

emptyA :: Auto a ()
emptyA = A {
  states = [()],
  initStates = [()],
  isAccepting = const False,
  transition = const $ const [] }

epsA :: Auto a ()
epsA = A {
  states = [()],
  initStates = [()],
  isAccepting = const True,
  transition = const $ const [] }

symA :: Eq a => a -> Auto a Bool
symA symbol = A {
  states = [True, False],
  initStates = [False],
  isAccepting = id,
  transition = trans
  }
  where trans True _ = []
        trans False sym | sym == symbol = [True]
        trans False sym | otherwise =  []

fromLeft :: Either a b -> a
fromLeft (Left l) = l

-- newTransition :: Either q r -> a -> [Either q r]
-- transtition :: q -> a -> [q] 

leftA :: Auto a q -> Auto a (Either q r)
leftA automata = A {
  states = map Left $ states automata,
  initStates = map Left $ initStates automata,
  isAccepting = (isAccepting automata) . fromLeft,
  transition = (\q a -> map Left (transition automata (fromLeft q) a))
  }

zipEither :: [a] -> [b] -> [Either a b]
zipEither ls rs = (map Left ls) ++ (map Right rs)

sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA autL autR = A {
  states = zipEither (states autL) (states autR),
  initStates = zipEither (initStates autL) (initStates autR),
  isAccepting = either (isAccepting autL) (isAccepting autR),
  transition = either (\q a -> map Left (transition autL q a)) (\q a -> map Right (transition autR q a))
  }

thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA aut1 aut2 = A {
  states = zipEither (states aut1) (states aut2),
  initStates = map Left $ states aut1,
  isAccepting = either (const False) (isAccepting aut2),
  transition = either leftTrans (\q a -> map Right $ transition aut2 q a)
  } where leftTrans leftState char = map Right rightStates ++ map Left nextStates
            where nextStates = transition aut1 leftState char
                  rightStates = if areAccepting aut1 nextStates then initStates aut2 else []
          
fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
fromLists states initStates acceptingStates transitions = A {
  states = states,
  initStates = initStates,
  isAccepting = (`elem` acceptingStates),
  transition = transition
  } where transition q a = concatMap (\(_,_,next) -> next) actTrans 
            where actTrans = filter isGood transitions
                  isGood (start,char,_) =  start == q && char == a

allValues :: (Enum a, Bounded a) => [a]
allValues = [minBound..maxBound]

crossProd :: [a] -> [b] -> [(a,b)]
crossProd listA listB = [(a, b) | a <- listA, b <- listB]

toLists :: (Enum a,Bounded a) => Auto a q -> ([q],[q],[q],[(q,a,[q])])
toLists automata = (
  states automata,
  initStates automata,
  filter (isAccepting automata) (states automata),
  transitions)
  where transitions = [(q, a, transition automata q a) | (q, a) <- statesChars ]  
        statesChars = (states automata) `crossProd` allValues
