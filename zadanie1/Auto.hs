module Auto (Auto, accepts, emptyA, epsA, symA, leftA, sumA, thenA, fromLists, toLists) where
import Data.List

data Auto a q = A { states      :: [q]
                  , initStates  :: [q]
                  , isAccepting :: q -> Bool
                  , transition  :: q -> a -> [q]
                  }

concatTransition :: Eq q => Auto a q -> [q] -> a -> [q]
concatTransition automata states char = nub newStates
  where newStates = concatMap (flip (transition automata) char) states

accepts :: Eq q => Auto a q -> [a] -> Bool
accepts automata chars = any (isAccepting automata) finalStates
  where finalStates = foldl (concatTransition automata) (initStates automata) chars

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

leftA :: Auto a q -> Auto a (Either q r)
leftA automata = A {
  states = map Left $ states automata,
  initStates = map Left $ initStates automata,
  isAccepting = either (isAccepting automata) (const False),
  transition = either (\q a -> map Left (transition automata q a)) (const $ const [])
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
  initStates = appendRight $ states aut1,
  isAccepting = either (const False) (isAccepting aut2),
  transition = either (\q a -> appendRight $ transition aut1 q a) (\q a -> map Right $ transition aut2 q a)
  }
  where
    appendRight states = (map Right rightStates) ++ map Left states
      where rightStates = if any (isAccepting aut1) states then initStates aut2 else []
          
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
