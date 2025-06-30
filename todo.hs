import Data.List
import System.Directory
import System.IO

main = do
  handle <- openFile "todo.txt" ReadMode
  contents <- hGetContents handle

  let cLines = lines contents
  let nLines = length cLines
  putStrLn "Which item would you like to remove?"
  mapM_
    (\(x, y) -> putStrLn (show x ++ " - " ++ y))
    (zip [1 .. nLines] cLines)
  putStrLn "Item: "

  response <- getLine
  let itemNum = read response - 1

  let newLines = delete (cLines !! itemNum) cLines

  tHandle <- openTempFile "." "todo.tmp"
  hPutStr (snd tHandle) (unlines newLines)
  hClose (snd tHandle)

  hClose handle
  renameFile (fst tHandle) "todo.txt"
