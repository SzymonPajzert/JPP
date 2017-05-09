module Main(main) where
import System.Environment
import Control.Exception
  
hmain = do
  [fileName] <- getArgs 
  putStrLn fileName


main =
  catch (hmain)
        (\e -> do
            let err = show (e :: IOException)
            putStrLn "hehe")
  
