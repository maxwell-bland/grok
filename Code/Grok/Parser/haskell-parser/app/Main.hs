module Main where

import Lib 
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Please supply a haskell file and parse type"
    [fileStr, parseType] -> do
      inputHaskellFileHandle <- openFile fileStr ReadMode
      haskellCode <- hGetContents inputHaskellFileHandle
      parseFile haskellCode parseType
      hClose inputHaskellFileHandle
    _ -> error "Please specify two arguments."
