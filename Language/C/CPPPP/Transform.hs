{-# LANGUAGE RankNTypes, ImpredicativeTypes, ExistentialQuantification #-}
module Language.C.CPPPP.Transform (transform) where

import qualified Language.C.Syntax as C
import qualified Data.Maybe as M
import System.IO
import Data.Loc

-- Import any transforms you want
import Language.C.CPPPP.Transforms.Int4
import Language.C.CPPPP.Transforms.Chs

-- | A transform is initialized, carries some state and is finalized.
data Transform = forall a . Transform { _init :: IO a, _trns :: a -> String -> [C.Id] -> SrcLoc -> IO (a, C.Initializer), _fin :: a -> IO () }

-- | All possible transforms and their identifiers
transforms :: [(String, Transform)]
transforms = [("c_hs", mkTransform mkChs), ("objc_hs", mkTransform mkObjChs), ("int_four", mkTransform mkInt4)]

-- Internal tools:
mkTransform (a, b, c) = Transform a b c
data LiveTransform = forall a . LiveTransform { _val :: a, _trns' :: a -> String -> [C.Id] -> SrcLoc -> IO (a, C.Initializer), _fini :: a -> IO () }
type Trns = [(String, LiveTransform)]

initTransforms :: IO Trns
initTransforms = mapM (\(s, Transform i t c) -> i >>= \x -> return (s, (LiveTransform x t c))) transforms

finTransforms :: Trns -> IO ()
finTransforms = mapM_ (\(_, LiveTransform s _ c) -> c s)

transform :: [C.Definition] -> IO [C.Definition]
transform xs = do
    ts <- initTransforms
    (s, t) <- mapOver transform' ts xs []
    finTransforms s
    return t

-- | Expressions must be in A-normal form so that they may be typed correctly in the FFI.
-- | In C, this means it is only valid as the initializer.
transform' :: Trns -> C.Definition -> IO (Trns, C.Definition)
transform' ts (C.FuncDef f l) = do
    (s, nf) <- newFunc f
    return (s, C.FuncDef nf l)
  where
    -- TODO: lens?...
    newFunc (C.Func de i dl p b s) = do
      (st, bs) <- mapOver appTrns ts b []
      return $ (st, C.Func de i dl p bs s)
transform' s x = return (s, x)

-- | Transform the insides of a BlockItem
appTrns :: Trns -> C.BlockItem -> IO (Trns, C.BlockItem)
appTrns ts (C.BlockDecl (C.InitGroup dcl ats ins loc)) = do
    (s, ins') <- mapOver initTrns ts ins []
    return $ (s, C.BlockDecl (C.InitGroup dcl ats ins' loc))
appTrns s x = return (s, x)

-- | Transform an initializer
initTrns :: Trns -> C.Init -> IO (Trns, C.Init)
initTrns ts (C.Init idx decl masm (Just initexp) attrs loc) = do
    (s, x) <- initExpTrns ts initexp
    return (s, C.Init idx decl masm (Just x) attrs loc)
initTrns s x = return (s, x)

-- | Lookup the quoter to use, then apply the expression and vars to it.
initExpTrns :: Trns -> C.Initializer -> IO (Trns, C.Initializer)
initExpTrns ts (C.QuotedInitializer (C.Id quoter _) expression vars loc) = do
  let lt = M.maybe (error "quoter not found") id (lookup quoter ts)
  (s, i) <- apply lt expression vars loc ts quoter
  return (s, i)
initExpTrns s x = return (s, x)

apply :: LiveTransform -> String -> [C.Id] -> SrcLoc
      -> [(String, LiveTransform)] -> String -> IO (Trns, C.Initializer)
apply (LiveTransform h f c) g i j ts qs = do
  (s, v) <- f h g i j
  return (update (\_ -> LiveTransform s f c) qs ts, v)

-- helper functions

update :: Eq k => (a -> a) -> k -> [(k, a)] -> [(k, a)]
update f key t
  = case t of
      [] -> []
      ((k, v):xs) -> case key == k of
          True  -> ((k, f v) : update f key xs)
          False -> ((k, v) : update f key xs)

mapOver fu sv [] cs = return (sv, cs)
mapOver fu sv (x:xs) cs = do
  (sv', x') <- fu sv x
  mapOver fu sv' xs (cs ++ [x'])
