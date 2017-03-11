module Main (main) where
import Auto
import Data.Char
import System.Environment
import Data.Maybe
import Control.Exception
import Text.Read

newtype Alpha = Alpha { unAlpha :: Char } deriving (Eq)
instance Bounded Alpha where
  minBound = Alpha 'A'
  maxBound = Alpha 'Z'
instance Enum Alpha where
  fromEnum = ord . unAlpha
  toEnum = Alpha . chr
instance Show Alpha where
  show = show . unAlpha

-- Note: do not use newtype to opaque the type inside record

data FileInput = FileInput { statesNumber :: Int
                           , initStates :: [Int]
                           , acceptingStates :: [Int]
                           , transitions :: [(Int, [Char], [Int])]
                           , word :: [Alpha]
                           }

newtype Transition = Transition (Int, [Char], [Int])
instance Read Transition where
  readsPrec _ line = [(Transition (read number, chars, map read statesList), "")]
    where number:chars:statesList = words line 

parseLine :: String -> Maybe (Int, [Char], [Int])
parseLine line = do
  Transition transition <- readMaybe line
  return transition

parseInput :: [String] -> Maybe FileInput
parseInput lines = do
  let _number:_initStates:_acceptingStates:_rest = lines
  let word:rest = reverse _rest

  let number = read _number
  let initStates = rList _initStates
  let acceptingStates = rList _acceptingStates
  
  return FileInput {
    statesNumber = number,
    initStates = initStates,
    acceptingStates = acceptingStates,
    transitions = map ((\(Just x) -> x) . parseLine) rest, 
    word = map Alpha word
    }

readFileInput :: FilePath -> IO FileInput
readFileInput fileName = do
  fileContent <- readFile fileName
  let Just fileInput = parseInput $ lines fileContent
  return fileInput

createAutomata :: FileInput -> Maybe (Auto Alpha Int)
createAutomata input =
  if validation
  then Just $ fromLists states (initStates input) (acceptingStates input) transitionList
  else Nothing
  where states = [1..(statesNumber input)]
        transitionList = func `concatMap` (transitions input)
        func (start, chars, ends) = (\char -> (start, Alpha char, ends)) `map` chars
        validation = True -- TODO implement

rList :: String -> [Int]
rList = read

hmain :: IO ()
hmain = do
  [fileName] <- getArgs
  fileInput <- readFileInput fileName
  let Just automata = createAutomata fileInput
  
  putStrLn $ show $ accepts automata $ word fileInput

main =
  catch (hmain) (\e -> do
      putStrLn $ show (e :: IOException)
      putStrLn "BAD INPUT")
