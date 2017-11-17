module Main where

import System.Environment

-- add line numbers to the beginning of each line

main = do
  xs <- getArgs
  case xs of
    [inFile, outFile] ->
      do file <- lines <$> readFile inFile
         let out = zipWith (\ n xs -> (show n) ++ ": " ++ xs) [1..] file
         writeFile outFile (unlines out)
    _ -> do putStrLn "Wrong number of arguments"
            prog <- getProgName
            putStrLn $ "Usage: " ++ prog ++ " inFile outFile"

