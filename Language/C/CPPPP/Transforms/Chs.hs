{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Language.C.CPPPP.Transforms.Chs (mkChs, mkObjChs) where

import Language.C.CPPPP.Transforms.CTypes
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Data.Loc (SrcLoc)
import Data.List
import Language.Haskell.TH hiding (varP)
import System.IO

mkChs = (initHs, mkChs', closeHs)
mkObjChs = (initHs', mkObjChs', closeHs)

mkChs' :: Handle -> String -> [C.Id] -> SrcLoc -> IO (Handle, C.Initializer)
mkChs' h ex vars loc = (fcall ex vars :: IO (FCall CLang)) >>= emit h loc

mkObjChs' :: Handle -> String -> [C.Id] -> SrcLoc -> IO (Handle, C.Initializer)
mkObjChs' h ex vars loc = (fcall ex vars :: IO (FCall ObjCLang)) >>= emit h loc

initHs = do
  h <- openFile "HSAux.hs" WriteMode
  hPutStrLn h "module HSAux where\n"
  return h
initHs' = do
  h <- openFile "HSAuxObjC.hs" WriteMode
  hPutStrLn h "module HSAuxObjC where\n"
  return h
closeHs = hClose

-- | Return the C expression and use IO to generate the auxiliary Haskell
-- | The auxiliary Haskell will use the FFI
-- It should also have a unique counter for generated function names (instead of Data.Unique).
-- | Write out the aux Haskell and return the C Expression to substitute
emit :: (FormatString a) => Handle -> SrcLoc -> FCall a -> IO (Handle, C.Initializer)
emit h loc (FCall fname ffi_name args) = do
    let carg_list = map snd args
        cexpr = C.FnCall (C.Var (C.Id ffi_name loc) loc) (map (flip C.Var loc) carg_list) loc
    hs_expr <- runQ $ mkHsExpr ffi_name fname args

    hPutStrLn h $ pprint hs_expr
    
    return (h, [cinit|$cexpr|])

-- args: [(CType CInt,Id "x" ),(CType CInt,Id "y" )]
mkHsExpr :: FormatString a => String -> String -> [(Arg a, C.Id)] -> Q [Dec]
mkHsExpr ffi_name fname args = do
  names <- mapM newName $ replicate (length args) "x"
  let types = applyT $ map (mkName . mkType . fst) args
      mar = map (mkMarshalling . fst) args
      body = NormalB $ applyN (zip mar (reverse names)) (mkName fname)
      for = ForeignD $ ExportF CCall ffi_name (mkName ffi_name) types
      decl = FunD (mkName ffi_name) [Clause (map VarP names) body []]
  return [for, decl]

applyN :: [(Exp, Name)] -> Name -> Exp
applyN [] fname = VarE fname
applyN [(m, x)] fname = AppE (VarE fname) (AppE m (VarE x))
applyN ((m, x):xs) fname = AppE (applyN xs fname) (AppE m (VarE x))

applyT :: [Name] -> Type
-- applyT [] = boom
applyT [x] = ConT x
applyT (x:xs) = AppT (AppT ArrowT (ConT x)) (applyT xs)
