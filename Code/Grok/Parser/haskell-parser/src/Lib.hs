module Lib
  ( parseFile
  ) where

import Data.List
import Language.Haskell.Exts.Parser

-- | parse
-- Parses a string representation of a haskell expression into
-- an AST
-- Show the parse if it was successful, or print the error.
-- TODO: Join these case statements together
parseFile :: String -> String -> IO ()
parseFile haskellExpr parseType =
  case parseType of
    "decl" ->
      case (parseDecl haskellExpr) of
        ParseOk expr -> putStr (show expr)
        ParseFailed srcLoc string ->
          putStr
            (concat
               (intersperse "\n" ["Parse Failed", (show srcLoc), string, ""]))
    "expr" ->
      case (parseExp haskellExpr) of
        ParseOk expr -> putStr (show expr)
        ParseFailed srcLoc string ->
          putStr
            (concat
               (intersperse "\n" ["Parse Failed", (show srcLoc), string, ""]))
