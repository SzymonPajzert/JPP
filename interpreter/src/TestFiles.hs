{-# OPTIONS_GHC -Wall -fwarn-tabs -fdefer-typed-holes #-}

module Main where

import Prelude hiding (log)
import System.Console.ANSI
import Control.Monad.Writer
import Data.Maybe (fromMaybe)

import DynGrammar
import Compile
import Util

import Err

testFilesPairs :: [(String, Maybe DynVal)]
testFilesPairs = [
  ("good/1.ad", Just $ TInt 5),
  ("good/2.ad", Just $ TInt 4),
  ("good/3.ad", Just $ TInt 5),
  ("good/fib.ad", Just $ TInt 8),
  ("good/mut_rec.ad", Just $ TInt 8),
  ("good/lazy_if.ad", Just $ TInt 0),
  ("good/call_by_need.ad", Nothing),
  ("good/closures.ad", Just $ TBool True)
  ]

log :: String -> Writer [String] ()
log string = tell [string]

helpTestFile :: String -> Maybe DynVal -> Writer [String] Bool
helpTestFile fileCont expectedResult =
  case parse fileCont >>= desugar_prog of
     Bad err -> do
       log "Compilation error:"
       log $ show err
       return False
     Ok expr -> do
       case interpret expr of
         Ok (EVal value) ->
           if True `fromMaybe` ((==value) `fmap` expectedResult) then do
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
           log $ show err
           log $ indent expr
           return False

testFile :: String -> Maybe DynVal -> IO ()
testFile file expectedResult = do
  fileCont <- readFile file
  let (result, logs) = runWriter $ helpTestFile fileCont expectedResult

  let color = if result then Green else Red
  setSGR [SetColor Foreground Vivid color]
  putStrLn $ "[" ++ file ++ "]"
  setSGR [Reset]

  mapM_ putStrLn logs

main :: IO ()
main = do
  mapM_ (uncurry testFile) testFilesPairs

