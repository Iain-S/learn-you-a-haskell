-- Remember to run this in prompt before loading:
-- :set -package mtl
import Control.Monad.Writer

-- As per my chat with chatGPT, you can think of lists as
-- data structures or as computation contexts.

-- Lists as a data structures
sumList [] = 0
sumList (x : xs) = x + sumList xs

-- or
sumList' :: (Num a) => [a] -> a
sumList' = foldr (+) 0

-- or
sumList'' :: [Int] -> Int
sumList'' = sum

-- Lists as computation
pairs :: [(Int, Int)]
pairs = do
  x <- [1, 2]
  y <- [10, 20]
  return (x, y)

-- or, without the syntactic sugar
pairs' :: [(Int, Int)]
pairs' = [1, 2] >>= (\x -> [10, 20] >>= \y -> return (x, y))

-- sumList''' :: [Int] -> Int
-- foldList
-- pairs' = [1,2] >>= (\x -> (x >>= \y -> [10,20]))

-- Note that the String and Int are backwards.
a :: Writer String Int
a = writer (3, "4")

b = a >>= \x -> writer (x + x, "5")

-- c is show-able, while b is not.
-- c should contain (6, "45")
c = runWriter b

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number:" ++ show x])

multiply = do
  a <- logNumber 77
  b <- logNumber 88
  return $ a * b

main = do
  print multiply
