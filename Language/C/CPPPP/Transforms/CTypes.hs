{-# LANGUAGE EmptyDataDecls, TemplateHaskell #-}
module Language.C.CPPPP.Transforms.CTypes where

import qualified Language.C.Syntax as C
import Data.Loc (SrcLoc)
import Data.Unique
import Text.ParserCombinators.Parsec
import Language.Haskell.TH hiding (varP)
import Language.Haskell.TH.Quote

-- A function call into Haskell from ObjC
data FCall a = FCall String          -- ^ Haskell function name
                     String          -- ^ Unique ffi function name
                     [(Arg a, C.Id)] -- ^ Arguments
                     deriving Show

-- 2 types for what kind of foreign language we want
-- kind Language = ...
data CLang
data ObjCLang

-- Foreign Types
data TypeVal = CInt | CString | CFloat | CPtr deriving Show
-- | What is the (ctype, haskell type) of this typeVal
-- typeVal :: TypeVal -> (String, String)

data Arg a = HSLit TypeVal String  -- ^ Haskell literal (by initializer) (TODO: not just strings)
           | CType TypeVal         -- ^ A foreign type (%d -> int, %f -> float, etc.)
        -- | HSFunc String         -- ^ Haskell function (by name) (if we want nested expressions)
           deriving Show

-- | Provide the parser for arguments, and it will parse arguments in:
-- | `funcname %d %f 4 "lol" %s`
class FormatString a where
  -- | Parse 1 argument
  argP :: Parser (Arg a)
  mkType :: Arg a -> String
  mkMarshalling :: Arg a -> Exp
  -- TODO: generate marshalling code

-- C
instance FormatString CLang where
  argP = varP (findTypeC)
     <|> (fmap (HSLit CString) word) -- TODO: use the read parser to work out type as well
  mkType (CType CInt) = "CInt"
  mkType (CType CString) = "CString"
  mkType (CType CFloat) = "CFloat"
  mkType (CType CPtr) = "Ptr ()"
  mkMarshalling (CType CInt) = VarE $ mkName $ "fromInteger"
  -- mkMarshalling (CType CString) = "CString"
  -- mkMarshalling (CType CFloat) = "CFloat"
  -- mkMarshalling (CType CPtr) = "Ptr ()"

-- Obj C
instance FormatString ObjCLang where
  argP = varP (\x -> findTypeC x <|> findTypeObjC x)
     <|> (fmap (HSLit CString) word)
  mkType (CType CInt) = "Ptr NSNumber"
  mkType (CType CString) = "Ptr NSString"
  mkType (CType CFloat) = "Ptr NSNumber"
  mkType (CType CPtr) = "Ptr NSObject"
  mkMarshalling (CType CInt) = VarE $ mkName $ "fromInteger"
  -- mkMarshalling (CType CString) = "CString"
  -- mkMarshalling (CType CFloat) = "CFloat"
  -- mkMarshalling (CType CPtr) = "Ptr ()"

-- | Function call from the quoter expression
fcall :: (FormatString a) => String -> [C.Id] -> IO (FCall a)
fcall ex vars = do
    hfid <- fmap hashUnique newUnique
    let (fname, args) = parseFormatString ex
        hsname = "hs_ffi_" ++ show hfid
    return $ FCall fname hsname $ zip (args) vars

-- | A parser to return the function name and list of argument types
parser :: (FormatString a) =>  Parser (String, [Arg a])
parser = do
  fn   <- word
  separator
  args <- sepBy1 argP separator
  return (fn, args)

parseFormatString :: (FormatString a) => String -> (String, [Arg a])
parseFormatString str = case (parse parser "" str) of
                          Left e -> error $ show e
                          Right x -> x

-- Parsec helpers
word :: Parser String
word = many1 (letter <|> char '(' <|> char ')' <|> char '+')
separator :: Parser ()
separator = skipMany1 space

-- | A typed parameter from a format string
-- varP ::  => Parser (Arg a)
varP :: (FormatString a) => (Char -> Parser (Arg a)) -> Parser (Arg a)
varP x = do
  char '%'
  anyChar >>= x

-- TODO: existential type?
findTypeC :: (FormatString a) => Char -> Parser (Arg a)
findTypeC 'd' = return $ CType CInt
findTypeC 's' = return $ CType CString
findTypeC 'f' = return $ CType CFloat
findTypeC  x  = fail   $ "unknown format string \"" ++ [x] ++ "\""

findTypeObjC :: Char -> Parser (Arg ObjCLang)
findTypeObjC '@' = return $ CType CPtr
findTypeObjC x   = fail   $ "unknown format string \"" ++ [x] ++ "\""
