module Lib
  ( parseFile
  ) where

import Data.List
import Language.Haskell.Exts.Parser

-- | parse
-- Parses a string representation of a haskell expression into
-- an AST
parseFile :: String -> IO ()
parseFile haskellExpr =
  case (parseExp haskellExpr)
    -- Show the parse if it was successful, or print the error.
        of
    ParseOk expr -> putStr (show expr)
    ParseFailed srcLoc string ->
      putStr
        (concat (intersperse "\n" ["Parse Failed", (show srcLoc), string, ""]))
