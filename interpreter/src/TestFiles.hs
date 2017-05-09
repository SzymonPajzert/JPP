{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -fdefer-typed-holes #-}

module Main where

import DynamicGrammar
import ErrM

testFilesPairs :: [(String, DynVal)]
testFilesPairs = [
  ("good/1.ad", TInt 5),
  ("good/2.ad", TInt 5),
  ("good/3.ad", TInt 5)
  ]



testFile :: String -> DynVal -> IO ()
testFile file result = do
  fileCont <- readFile file
  case parse fileCont >>= desugar_prog of
    Bad err -> do
      putStrLn "Compilation error:"
      putStrLn err
    Ok expr -> do
      putStrLn $ show expr
      case interpret expr of
        Ok value -> if value == result
          then putStrLn "Ok"
          else putStrLn $ "Wrong answer " ++ (show value) ++ " " ++ (show result)
        Bad err -> do
          putStrLn "Runtime error: "
          putStrLn err
      -- case run

main :: IO ()
main = do
  mapM_ (uncurry testFile) testFilesPairs

