import System.IO

mainClose = do
  handle <- openFile "girlfriend.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
  handle <- openFile "girlfriend.txt" ReadMode
  result <- f handle
  hClose handle
  return result

main = do
  withFile'
    "girlfriend.txt"
    ReadMode
    ( \x -> do
        contents <- hGetContents x
        putStr contents
    )
