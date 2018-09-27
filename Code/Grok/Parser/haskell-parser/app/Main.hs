module Main where

import Lib
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Please supply a haskell file to parse"
    [arg] -> do
      inputHaskellFileHandle <- openFile arg ReadMode
      haskellCode <- hGetContents inputHaskellFileHandle
      parseFile haskellCode
      hClose inputHaskellFileHandle
    _ -> error "Please specify only one argument."
