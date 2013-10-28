import Language.C.CPPPP
import System.Environment (getArgs)
import Control.Monad (mapM)

-- | Parse the file in the argument, transform it, then pretty print it.
main :: IO ()
main = do
    args <- getArgs
    let fname = head args
    x <- parseFile fname
    newX <- transform x
    writeFile ("T_" ++ fname) $ prettyPrint newX
