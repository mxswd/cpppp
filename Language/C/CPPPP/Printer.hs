module Language.C.CPPPP.Printer (prettyPrint) where

import Text.PrettyPrint.Mainland

-- | Typical pretty print.
prettyPrint :: Pretty a => a -> String
prettyPrint x = prettyPragma 80 (ppr x)
