-- Reverse Polish Notation
import System.Environment

-- So that we can inspect bad results.
data Result = Good Integer | Bad [Integer] deriving (Show)

evalRPN :: Num a => String -> a
evalRPN s = 1

-- operators :: [(String, (Num a) => (a -> a -> a))]
operators :: [(String, Integer -> Integer -> Integer)]
operators = [("-", (-)),("*", (*)),("+", (+))]

main = do
    args <- getArgs
    print $ evalRec [] (words (head args))
    

evalRec :: [Integer] -> [String] -> Result
evalRec [] (s:ss) = evalRec [read s] ss
evalRec [result] [] = Good result
evalRec x [] = Bad x -- Shouldn't happen, but did.
evalRec (x:xs) (s:ss) = 
    case lookup s operators of 
        Just op -> evalRec (head xs `op` x: tail xs) ss
        Nothing -> evalRec (read s:(x:xs)) ss
    
          