-- import Data.Foldable

import Control.Monad (MonadPlus (mzero))
import qualified Data.Foldable as F
import Data.Monoid

-- A `newtype` can only have one value constructor
-- and that can only have one field.
newtype CharList = CharList {getCharList :: [Char]} deriving (Eq, Show)

-- Make a CharList and convert it back to a String
x = getCharList (CharList "ABC")

-- If we want fmap to change the first component
-- in a tuple, we can use newtype
newtype Pair b a = Pair {getPair :: (a, b)}

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

mapped = fmap (+ 1) (Pair (1, 2))

-- One way to compare strings
lenComp a b =
  let x = length a `compare` length b
      y = a `compare` b
   in if x == EQ then y else x

lenComp' a b = (length a `compare` length b) `mappend` (a `compare` b)

type Maybe' = Maybe

-- "Duplicate instance" error
-- instance Monoid a => Monoid (Maybe' a) where

result = F.foldl (+) 2 (Just 2)

data Tree v = Empty | Node v (Tree v) (Tree v)

instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) =
    F.foldMap f l
      `mappend` f x
      `mappend` F.foldMap f r

testTree =
  Node
    5
    ( Node
        3
        (Node 1 Empty Empty)
        (Node 6 Empty Empty)
    )
    ( Node
        9
        (Node 8 Empty Empty)
        (Node 10 Empty Empty)
    )

-- folded = foldl (+) 0 testTree
summed = sum testTree

anyThree = getAny $ F.foldMap (\x -> Any $ x == 3) testTree

type Birds = Int

type Pole = (Birds, Birds)

landLeft' :: Birds -> Pole -> Pole
landLeft' b (l, r) = (l + b, r)

landRight' :: Birds -> Pole -> Pole
landRight' b (l, r) = (l, r + b)

-- An infix function application
x -: f = f x

nf = False -: not

pole = (0, 0) -: landLeft' 1 -: landRight' 1 -: landLeft' 2

-- using Maybe! Let's rework these functions:

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

-- And now we use our bind operator
poleTwo = return (0, 0) >>= landLeft 1 >>= landRight 3 >>= landRight 2 >>= landRight (-2)

-- todo : see how far I can get getting a similar result using applicatives

foo = do
  x <- Just 2
  y <- Just "!"
  Just (show x ++ y)

-- do equivalents
equiv =
  Just 3
    >>= ( \x ->
            Just "!"
        )
    >>= ( \y ->
            Just (show x ++ y)
        )

orDo = do
  x <- Just 3
  y <- Just "!"
  -- Nothing
  return $ show x ++ y

bopbop = do
  (x : xs) <- Just ""
  return x

-- Monads and lists
l = [1, 2] >>= (\x -> [x, -x])

-- return puts something in a minimal context
ll = [1, 2] >>= \x -> ["a", "b"] >>= \y -> return x

ii = [1, 2] >>= \x -> ["a", "b"] >>= \y -> [x]

-- "list comprehensions are just syntactic sugar
--  for using lists as monads"
zz = [(n, ch) | n <- [1, 2], ch <- ["a", "b"]]

zx = [1, 2] >>= \x -> ["a", "b"] >>= \y -> return (x, y)

zc = zz `compare` zx -- EQ

-- some filtering
sevens = [x | x <- [1 .. 50], '7' `elem` show x]

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

-- guarding with do
sevensOnly = do
  x <- [0 .. 50]
  guard ('7' `elem` show x)
  return x

-- guarding with >>
zevensOnly = [1 .. 50] >>= (\x -> guard ('7' `elem` show x) >> return x)

-- Chess
type Pos = (Int, Int)

-- Possible moves
moveKnight :: Pos -> [Pos]
moveKnight (c, r) = do
  (c', r') <-
    [ (c + 2, r - 1),
      (c + 2, r + 1),
      (c - 2, r - 1),
      (c - 2, r + 1),
      (c + 1, r - 2),
      (c + 1, r + 2),
      (c - 1, r - 2),
      (c - 1, r + 2)
    ]
  guard (elem c rang && elem r rang)
  return (c', r')
  where
    rang = [1 .. 8]

-- using filter instead
moveKnight' (c, r) =
  filter
    onBoard
    [(c + a * x, r + b * y) | a <- ys, b <- ys, x <- xs, y <- xs, x /= y]
  where
    xs = [1, 2]
    ys = [1, -1]
    onBoard (c, r) = inBoard c && inBoard r
    inBoard c = c `elem` [1 .. 8]

-- All positions reachable in three moves.
