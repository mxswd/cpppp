{-# LANGUAGE OverloadedStrings #-}
import Language.C.CPPPP
import System.Environment (getArgs)
import Control.Monad (mapM)
import Data.String

import Language.C.Syntax
import Language.C.Parser
import Data.Loc

l = fromPos $ startPos "tests"

instance IsString Id where
  fromString = flip Id l

c_code :: String -> String -> [Id] -> [Definition]
c_code quoter expression vars = [
  FuncDef (
  Func (DeclSpec [] [] (Tint Nothing l) l) (Id "main" l) (DeclRoot l)
  (Params [Param Nothing (DeclSpec [] [] (Tvoid l) l) (DeclRoot l) l] False l) [
  
    -- int x = 2;
    BlockDecl (InitGroup (DeclSpec [] [] (Tint Nothing l) l) []
      [Init (Id "x" l) (DeclRoot l) Nothing
      (Just (ExpInitializer (Const (IntConst "2" Signed 2 l) l) l)) [] l] l),
    
    -- int y = 4;
    BlockDecl (InitGroup (DeclSpec [] [] (Tint Nothing l) l) []
      [Init (Id "y" l) (DeclRoot l) Nothing
      (Just (ExpInitializer (Const (IntConst "4" Signed 4 l) l) l)) [] l] l),
    
    -- int z = @|(+) %d %d|x, y|;
    BlockDecl (InitGroup (DeclSpec [] [] (Tint Nothing l) l) []
      [Init (Id "z" l) (DeclRoot l) Nothing
      -- this is where the magic happens
      (Just (QuotedInitializer (Id quoter l) expression vars l)) [] l] l),
    
    --  printf("%d %f", z);
    BlockStm (Exp (Just (FnCall (Var (Id "printf" l) l)
      [Const (StringConst ["\"%d\""] "%d" l) l,Var (Id "z" l) l] l)) l),
    
    -- return 0;
    BlockStm (Return (Just (Const (IntConst "0" Signed 0 l) l)) l)] l)
    
  l]

main :: IO ()
main = do
    -- code <- parseFile "Tests/MXViewController.m"
    code <- parseFile "Tests/example.c"
    -- newX <- mapM transform x
    -- let code = c_code "int_four" "" []
    -- let code = c_code "c_hs" "(+) %d %d" ["x", "y"] -- @|chs|(+) %d %y|x, y|
    -- let code = c_code "objc_hs" "" []
    newCode <- transform code
    putStrLn $ prettyPrint newCode
