module Language.C.CPPPP.Parser (parseFile) where

import qualified Language.C.Syntax as C
import qualified Language.C.Parser as P
import qualified Data.ByteString.Char8 as B
import Data.Loc

-- | Parses C / Obj-C files with the quoted initializer extension.
parseFile :: String -> IO [C.Definition]
parseFile filename = do
    let exts = [C.ObjC] -- TODO: Add C.QInit when supported
    s <- B.readFile filename
    case P.parse exts [] P.parseUnit s start of
      Left err   -> error $ show err
      Right defs -> return defs
  where
    start :: Pos
    start = startPos filename
