import Data.List
import System.Directory
import System.Environment
import System.IO

removeTodo [] = putStrLn "Usage: todo.hs <todofile.txt>"
removeTodo [fileName] = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle

  let cLines = lines contents
  let nLines = length cLines
  putStrLn "Which item would you like to remove?"

  -- Note that this could have been a zipWith instead.
  mapM_
    (\(x, y) -> putStrLn (show x ++ " - " ++ y))
    (zip [1 ..] cLines)
  putStrLn "Item: "

  response <- getLine
  if response == ""
    then
      return ()
    else do
      let itemNum = read response - 1

      let newLines = delete (cLines !! itemNum) cLines

      tHandle <- openTempFile "." "todo.tmp"
      hPutStr (snd tHandle) (unlines newLines)
      hClose (snd tHandle)

      hClose handle
      renameFile (fst tHandle) fileName
removeTodo (x : xs) = putStrLn "Usage: todo.hs <todofile.txt>"

main = do
  args <- getArgs
  removeTodo args
