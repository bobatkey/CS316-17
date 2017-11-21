module Main where

import System.Environment

-- add line numbers to the beginning of each line

numberLines =
  unlines . map (\(i,l) -> show i ++ ":" ++ l) . zip [1..] . lines

main :: IO ()
main = interact numberLines

  -- do
  -- args <- getArgs
  -- case args of
  --   [inputFile, outputFile] -> do
  --     numberLines <$> readFile inputFile >>= writeFile outputFile
  --   _ -> do
  --     prog <- getProgName
  --     putStrLn ("Usage: " ++ prog ++ " <inputFile> <outputFile>")
