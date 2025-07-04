import Control.Exception
import System.Environment
import System.IO.Error

-- Fails if we can't lookup the value in the list.
mainLookup = do
  print $ takesVal $ lookup 3 [(1, 2)]

takesVal (Just x) = x

-- Fails if the file doesn't exist.
mainRaises = tryTo

-- Catches the exception if the file doesn't exist.

main = tryTo `catch` handler

tryTo = do
  (filename : _) <- getArgs
  contents <- readFile filename
  putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines."

-- Only try to handle doesNotExist errors.
handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e = putStrLn "Ooops, an error happened"
  | otherwise = ioError e
