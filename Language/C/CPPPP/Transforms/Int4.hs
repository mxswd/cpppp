{-# LANGUAGE QuasiQuotes #-}
module Language.C.CPPPP.Transforms.Int4 (mkInt4) where

-- You can build an AST or use the quasi quoter.
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Data.Loc (SrcLoc)

mkInt4 :: (IO (),
      () -> String -> [C.Id] -> SrcLoc -> IO ((), C.Initializer),
      () -> IO ())
mkInt4 = (return (), mkInt4', const . return $ ())

mkInt4' :: () -> String -> [C.Id] -> SrcLoc -> IO ((), C.Initializer)
mkInt4' _ expression vars loc = return ((), [cinit|4|])
  -- return (C.ExpInitializer (C.Const (C.IntConst "4" C.Signed 4 loc) loc) loc)
