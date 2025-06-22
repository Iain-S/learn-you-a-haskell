import Distribution.Simple.Utils (xargs)

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
