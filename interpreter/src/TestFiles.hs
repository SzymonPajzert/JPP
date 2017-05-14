{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -fdefer-typed-holes #-}

module Main where

import System.Console.ANSI
import DynamicGrammar
import ErrM

testFilesPairs :: [(String, DynVal)]
testFilesPairs = [
  ("good/1.ad", TInt 5),
  ("good/2.ad", TInt 5),
  ("good/3.ad", TInt 5),
  ("good/fib.ad", TInt 8)
  ]


testFile :: String -> DynVal -> IO ()
testFile file expectedResult = do
 
  fileCont <- readFile file
  result <- (case parse fileCont >>= desugar_prog of
    Bad err -> do
      putStrLn "Compilation error:"
      putStrLn err
      return False
    Ok expr -> do
      putStrLn $ show expr
      case interpret expr of
        Ok value -> if value == expectedResult
          then putStrLn "Ok" >> return True
          else (putStrLn $ "Wrong answer " ++ (show value) ++ " instead of " ++ (show expectedResult))
             >> return False
        Bad err -> do
          putStrLn "Runtime error: "
          putStrLn err
          return False)
  let color = if result then Green else Red
  setSGR [SetColor Foreground Vivid color]
  putStrLn $ "[" ++ file ++ "]"
  setSGR [Reset]


main :: IO ()
main = do
  mapM_ (uncurry testFile) testFilesPairs

