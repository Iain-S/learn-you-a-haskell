doubleMe x = x + x

doubleSmallNum x = if x > 100 then x else x * 2

lostNumbers = [1, 2, 3]

comprehension = [x * 2 | x <- [1 .. 10], x * 2 >= 12]

boonBang xs = [if x < 10 then "BOOM" else "BANG" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

triangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. 10], a <- [1 .. 10]]

rightTriangles :: [(Integer, Integer, Integer)]
rightTriangles =
  [ (a, b, c)
  | c <- [1 .. 10],
    b <- [1 .. 10],
    a <- [1 .. 10],
    a ^ 2 == (b ^ 2 + c ^ 2),
    a + b + c == 24
  ]

-- Explicit type hints.
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

ident :: a -> a
ident x = x

-- Constrained with a "class constraint"
ident' :: (Num a) => a -> a
ident' x = x

-- Guess the type of head
-- takes a list of things and returns a thing
-- Commented out to silence the warning.
-- head' :: [a] -> a
-- head' xs = head xs

-- The type of the == operators is
-- (==) :: Eq a => a -> a -> Bool

-- pattern matching
-- note that the order matters
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY SEVEN"
lucky x = "UNLUCKY"

-- like fst, but for triples
first :: (a, b, c) -> a
first (x, _, _) = x

headd :: [a] -> a
headd [a] = a
headd [] = error "nope"
headd (x : _) = x

-- the above were patterns, these are guards
max' a b
  | a < b = b
  | otherwise = a

-- Note the indentation of the air and water
densityTell mazz volume
  | density < air = "Float"
  | density <= water = "Swimming"
  | otherwise = "Sinking!"
  where
    density = mazz / volume
    air = 1.2
    water = 1000.0

a = densityTell 1.0 2.0

-- This is quite neat!
calcDensities xs = [density m v | (m, v) <- xs]
  where
    density mass volume = mass / volume

cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * (r ^ 2)
   in sideArea + 2 * topArea

calcDensities' xs = [m / v | (m, v) <- xs]

calcDensitoy' xs = [density | (m, v) <- xs, let density = m / v, density < 1.2]

calcDensity xs = [density | (m, v) <- xs, let density = m / v, density < 1.2]

-- Case expressions
describeList :: [a] -> String
describeList xs =
  "The list is " ++ case xs of
    [] -> "empty"
    [_] -> "singleton"
    _ -> "two elements or more"

myMaximum :: (Ord a) => [a] -> a
myMaximum [a] = a
myMaximum (x : xs)
  | x > rest = x
  | otherwise = rest
  where
    rest = myMaximum xs

maximum' :: (Ord a) => [a] -> a
maximum' [x] = x
maximum' (x : xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = maximum' xs

replicate' x y
  | x == 0 = []
  | otherwise = y : replicate' (x - 1) y

take' :: (Integral a) => a -> [b] -> [b]
take' _ [] = []
take' 0 xs = []
take' y (x : xs) = x : take' (y - 1) xs

-- Weirdly, this still doesn't seem to work with n
taker :: (Integral a) => a -> [b] -> [b]
taker n xs
  | n <= 0 = []
taker _ [] = []
taker n (x : xs) = x : take' (n - 1) xs

-- polymorphism?
-- poly a = a
-- poly a b = b
-- no.

elem' :: (Eq a) => a -> [a] -> Bool
elem' x [] = False
-- Not in Kansas now, Dorothy.
-- elem' x (y: ys) = y == x || elem' x ys
elem' x (y : ys)
  | y == x = True
  | otherwise = elem' x ys

-- quicksort
-- a sorted list, a middle, a sorted list
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
-- quicksort' [a] = [a]
-- quicksort' [a,b] = [a,b]
-- quicksort' (x:xs) = [i ++ (j : k) | let i = [1], let j = 2, let k = [3]]
quicksort' (x : xs) = [y | y <- xs, y < x] ++ (x : [y | y <- xs, x <= y])

-- That's quite interesting.
quicksort_ :: (Ord a) => [a] -> [a]
quicksort_ [] = []
quicksort_ (x : xs) = less ++ (x : greater)
  where
    less = quicksort_ [y | y <- xs, y < x]
    greater = quicksort_ [y | y <- xs, x <= y]

-- Can partially apply infix functions with (). E.g.
b = (/ 10) 100

c = (10 /) 100

-- Higher order
-- applyTwice ::
applyTwice f x = f (f x)

-- alt
appliesTwice f = f . f

getDouble x = doubleMe

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- Reverse the order of the arguments.
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x

-- Simpler implementation
flip_ :: (a -> b -> c) -> b -> a -> c
flip_ f y x = f x y

-- Alternative implementation, which uses filter (a h.o.f.)
quicksorted [] = []
quicksorted (x : xs) = sortedLeft ++ x : sortedRight
  where
    sortedLeft = quicksorted (filter (< x) xs)
    sortedRight = quicksorted (filter (>= x) xs)

-- Make a list of multiplicative functions and apply one of them
somefunc = map (*) [1 ..] !! 5

-- Lambdas
-- These two are equivalent
withoutLambdas :: (Num a) => a -> a -> a -> a
withoutLambdas a b c = a + b + c

withLambdas :: (Num a) => a -> a -> a -> a
withLambdas = \x -> \y -> \z -> x + y + z

-- foldl (fold from the left/reduce)
-- foldl (\acc x -> acc + x) 19 [1,5..20]

folde e = foldl (\acc x -> acc || (x == e)) False

-- Note that the lambda params have swapped.
forde e = foldr (\x acc -> if x == e then True else acc) False

-- e.g. folde 5 [1..10]

map' f = foldr (\x acc -> (f x) : acc) []

-- todo
-- maximum_
maximum_ :: (Ord a) => [a] -> a
maximum_ = foldl1 max

-- product_
product_ :: (Num a) => [a] -> a
product_ = foldl1 (*)

-- head_ (this is silly)
head_ :: [a] -> a
head_ = foldl1 const

-- not sure I understand why this works
head__ :: [a] -> a
head__ = foldr1 const

-- last_
last_ :: [a] -> a
last_ = foldl1 (flip const)

-- filter_
filter_ expression = foldr (\x acc -> if expression x then x : acc else acc) []

-- reverse_
reverse_ :: [a] -> [a]
reverse_ = foldl (flip (:)) []

p :: Integer -> Integer -> Integer
p = flip (+)

-- scanl and scanr are like foldl and foldr,
-- only they report all the intermediate accumulator states in the form of a list.
-- scanl (+) 0 [1..10]

maybeadd :: (Num b) => Maybe b -> b -> Maybe b
maybeadd x y = x >>= (\x -> Just $ x + y)

-- maybeadd Nothing 2
-- maybeadd (Just 1) 2

-- From https://www.youtube.com/watch?v=IBB7JpbClo8
monadd mx my = do
  x <- mx
  y <- my
  return (x + y)

-- monadd (Just 100) (Just 1.0)
