import Control.Monad
main = do
  putStrLn "Hello, what's your name?"
  line <- getLine
  if line == "" then
    return subMain -- doesn't do anything useful
    else do
      putStrLn $ reverseWords line
      main

reverseWords :: String -> String
reverseWords = unwords . reverse . words

-- Return makes an I/O out of a pure value. 
-- It DOES NOT stop execution by returning from a function.
subMain = do
  a <- return "hi"
  b <- return "there"
  putStrLn $ a ++ b

altMain = do
  c <- getChar
  -- Have to import for when.
  when (c /= ' ') $ do
    putChar c
    altMain

seqMain = do
  rs <- sequence [getChar, getChar, getChar]
  print rs


