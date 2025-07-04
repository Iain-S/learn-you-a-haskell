import Control.Monad

-- Think of forM as making an I/O action for every element
-- in the list.
main = do
  result <-
    forM
      [1, 2, 3]
      ( \x -> do
          putStrLn $ "What colour for " ++ show x ++ "?"
          getLine
      )
  putStrLn "The colours you associate are:"
  mapM putStrLn result

-- Don't think of a function like putStrLn as a function
-- that takes a string and prints it to the screen.
-- Think of it as a function that takes a string and returns
-- an I/O action. That I/O action will, when performed,
-- print beautiful poetry to your terminal.
