{-# OPTIONS_GHC -Wall -fwarn-tabs -fdefer-typed-holes #-}

module Main where

import Prelude hiding (log)
import System.Console.ANSI
import Control.Monad.Writer
import DynamicGrammar
import ErrM

testFilesPairs :: [(String, DynVal)]
testFilesPairs = [
  ("good/1.ad", TInt 5),
  ("good/2.ad", TInt 4),
  ("good/3.ad", TInt 5),
  ("good/fib.ad", TInt 8),
  ("good/mut_rec.ad", TInt 8),
  ("good/lazy_if.ad", TInt 0)
  ]

log :: String -> Writer [String] ()
log string = tell [string]

helpTestFile :: String -> DynVal -> Writer [String] Bool
helpTestFile fileCont expectedResult =
  case parse fileCont >>= desugar_prog of
     Bad err -> do
       log "Compilation error:"
       log err
       return False
     Ok expr -> do
       case interpret expr of
         Ok (EVal value) ->
           if value == expectedResult then do
               log "Ok"
               return True
           else do
             log $ "Wrong answer " ++ (show value) ++ " instead of " ++ (show expectedResult)
             log $ indent expr
             return False
         Ok expr -> do
           log "Bad return value"
           log $ indent expr
           return $ False
         Bad err -> do
           log "Runtime error: "
           log err
           log $ indent expr
           return False

-- TODO change to monad writer to save the logs, print header in right color and print logs.
testFile :: String -> DynVal -> IO ()
testFile file expectedResult = do
  fileCont <- readFile file
  let (result, logs) = runWriter $ helpTestFile fileCont expectedResult

  let color = if result then Green else Red
  setSGR [SetColor Foreground Vivid color]
  putStrLn $ "[" ++ file ++ "]"
  setSGR [Reset]

  mapM_ putStrLn logs

  {-
  result <- (case parse fileCont >>= desugar_prog of
    Bad err -> do
      putStrLn "Compilation error:"
      putStrLn err
      return False
    
  -}

main :: IO ()
main = do
  mapM_ (uncurry testFile) testFilesPairs

