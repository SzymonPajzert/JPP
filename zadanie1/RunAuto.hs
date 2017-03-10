module Main (main) where
import Auto
import Data.Char
import System.Environment

newtype Alpha = Alpha { char :: Char } deriving (Eq)
instance Bounded Alpha where
  minBound = Alpha 'A'
  maxBound = Alpha 'Z'
instance Enum Alpha where
  fromEnum = ord . char
  toEnum = Alpha . chr
instance Show Alpha where
  show = show . char

-- Note: do not use newtype to opaque the type inside record

data FileInput = FileInput {
  statesNumber :: Int,
  initStates :: [Int],
  acceptingStates :: [Int],
  transitions :: [(Int, [Char], [Int])],
  word :: String
  }

parseInput :: [String] -> IO FileInput
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
    transitions = map parse rest, 
    word = word
    }
    where parse line = (0, [], [0]) -- TODO implement


readFileInput :: FilePath -> IO FileInput
readFileInput fileName = do
  fileContent <- readFile fileName
  fileInput <- parseInput $ lines fileContent
  return fileInput

createAutomata :: FileInput -> Maybe (Auto Char Int)
createAutomata input =
  if validation
  then Just $ fromLists states (initStates input) (acceptingStates input) transitionList
  else Nothing
  where states = [1..(statesNumber input)]
        transitionList = func `concatMap` (transitions input)
        func (start, chars, ends) = (\char -> (start, char, ends)) `map` chars
        validation = True -- TODO implement


rList :: String -> [Int]	  
rList = read

main = do
  fileName:[] <- getArgs
  fileInput <- readFileInput fileName
  let Just automata = createAutomata fileInput
  
  putStrLn $ show (accepts automata (word fileInput))
