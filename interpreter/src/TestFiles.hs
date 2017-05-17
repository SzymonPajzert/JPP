{-# OPTIONS_GHC -Wall -fwarn-tabs -fdefer-typed-holes #-}

module Main where

import Prelude hiding (log)
import System.Console.ANSI
import Control.Monad.Writer

import DynGrammar
import Compile
import Util

import Err

testFilesPairs :: [String]
testFilesPairs = [
  "good/1.ad",
  "good/2.ad",
  "good/3.ad",
  "good/fib.ad",
  "good/mut_rec.ad",
  "good/lazy_if.ad",
  "good/closures.ad",
  "good/fast_fib.ad"
  
  -- "good/call_by_need.ad"
  ]

log :: String -> Writer [String] ()
log string = tell [string]

helpTestFile :: String -> Writer [String] Bool
helpTestFile fileCont =
  case parse fileCont >>= desugar_prog of
     Bad err -> do
       log "Compilation error:"
       log $ show err
       return False
     Ok prog -> do
       case interpret prog of
         Ok (EBool True) -> do
           log "Ok"
           return True
         Ok expr -> do
           log "Bad return value"
           log $ indent expr
           return $ False
         Bad err -> do
           log "Runtime error: "
           log $ show err
           log $ indent prog
           return False

testFile :: String -> IO ()
testFile file = do
  fileCont <- readFile file
  let (result, logs) = runWriter $ helpTestFile fileCont

  let color = if result then Green else Red
  setSGR [SetColor Foreground Vivid color]
  putStrLn $ "[" ++ file ++ "]"
  setSGR [Reset]

  mapM_ putStrLn logs

main :: IO ()
main = do
  mapM_ testFile testFilesPairs

