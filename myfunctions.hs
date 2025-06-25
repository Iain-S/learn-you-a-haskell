import Data.List
import Data.Map (Map, fromList, toList)

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
maybeadd x y = x >>= (\x -> Prelude.Just $ x + y)

-- maybeadd Nothing 2
-- maybeadd (Just 1) 2

-- From https://www.youtube.com/watch?v=IBB7JpbClo8
monadd mx my = do
  x <- mx
  y <- my
  return (x + y)

-- monadd (Just 100) (Just 1.0)

-- Can we define our own infix function?
-- (blob) :: (a -> b) -> a -> b
-- f blob x = f x
-- not easily

-- $ is often used for convenience, to remove the need for brackets

aaa = sqrt $ 3 + 4 + 9

aab = sqrt (3 + 4 + 9)

aac = sum (filter (> 10) (map (* 2) [2 .. 10]))

aad = sum $ filter (> 10) $ map (* 2) [2 .. 10]

-- function composition
allNegative = map (\x -> negate $ abs x)

allNegative_ = map $ negate . abs

repli = (sum . replicate 5 . max 6.7)

-- point free style
somefu x = ceiling (negate (tan (cos (max 50 x))))

otherfu = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

oddSquareSum' = sum . takeWhile (< 1000) $ filter odd . map (^ 2) $ [1 ..]

somenumbs = intersperse 5 [1 .. 10]

-- More on modules todo

-- Data types and typeclasses
data Shape
  = Circle Float Float Float
  | Rect Float Float Float Float
  deriving (Show)

-- Note that Circle and Rect are not types but Shape is.
surfaceArea (Circle _ _ r) = pi * r ^ 2
surfaceArea (Rect x1 y1 x2 y2) = abs (x2 - x1) * abs (y2 - y1)

-- Can display with
-- show (Circle 10 0 0)

-- Notice that, for Point, we use the same name for the data type
-- and for the value constructor, which is common if there is only one constructor.
data Point = Point Float Float deriving (Show)

-- Notice here that there are two constructors and neither has the name of the type.
data Shap = Circular Point Float | Rectangle Point Point deriving (Show)

-- Using "record syntax"
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

mustang = Car "Ford" "Mustang" 1067

-- can now do
-- ghci> company mustang

data Maybee a = Nothing | Just a

-- Maybee is a type constructor.
-- No value can have a type of Maybee but could
-- have a type of Maybee Int.
-- ghci> :k Maybee Int
-- Maybee Int : *
-- ghci> :t Just 1
-- Just 1 :: Num a => Maybee a

-- A 3D Vector type
data Vector a = Vector a a a deriving (Show)

(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (m + j) (n + k)

-- Use of the deriving to make Person and instance of the Eq typeclass.
data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }
  deriving (Eq, Show, Read)

md = read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person

-- Because all of the value constructors are nullary (take no params)
-- we can use Enum, a typeclass for things with predecessors and successors.
-- Bounded is for things that have a lowest and highest possible value.
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Note that minBound doesn't seem to be a function
-- ghci> minBound :: Day

-- type for type aliases
type PhoneNumber = String

type Name = String

type Entry = (Name, PhoneNumber)

type PhoneBook = [Entry]

inPhoneBook :: Entry -> PhoneBook -> Bool
inPhoneBook e p = e `elem` p

-- We can parameterise type synonyms too:
type AssocList k v = [(k, v)]

-- Did they just sneakily introduce monands?
getFromAssocList :: (Eq k) => k -> AssocList k v -> Maybe v
getFromAssocList _ [] = Prelude.Nothing
getFromAssocList k (x : xs) = if k == fst x then Prelude.Just (snd x) else getFromAssocList k xs

xy Prelude.Nothing = 1
xy (Prelude.Just a) = a

-- Some recursive data structures
-- A tree is either an empty tree or a node with two tree branches.
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singletonTree x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singletonTree x
treeInsert x (Node a t1 t2)
  | x == a = Node x t1 t2
  | x < a = Node a (treeInsert x t1) t2
  | otherwise = Node a t1 (treeInsert x t2)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a t1 t2)
  | x == a = True
  | x < a = treeElem x t1
  | otherwise = treeElem x t2

-- data is for defining new types
-- class is for typeclasses (kinda like interfaces)
data Fruit = Apple | Banana deriving (Show)

instance Eq Fruit where
  Apple == Apple = True
  Banana == Banana = True
  _ == _ = False

-- Just because we can.
-- we'll make our Fruit "an instance" of the Num typeclass.
instance Num Fruit where
  (+) a b = Apple
  (-) a b = Apple
  (*) a b = Apple
  negate a = if a == Apple then Banana else Apple
  abs a = a
  signum a = a
  fromInteger i = Banana

-- "We want all types of the form Maybee m to be
-- part of the Eq typeclass, but only those types
-- where the m is also part of Eq."
instance (Eq m) => Eq (Maybee m) where
  Main.Just x == Main.Just y = x == y
  Main.Nothing == Main.Nothing = True
  _ == _ = False

-- A YesNo typeclass (truthy thing)
class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [unused] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

-- Is this smart? Probably not.
instance (YesNo a) => YesNo (Maybee a) where
  yesno Main.Nothing = False
  yesno (Main.Just x) = yesno x

-- And now, we're going to take a look at the Functor typeclass,
-- which is basically for things that can be mapped over.
-- Note that functor is for things that implement fmap,
-- it isn't for functions.

-- In fact, map is just a fmap that works only on lists. !

-- Can we make Data.Map's Map an instance of Functor?

-- This is left as an exercise but not a very clear one.
-- We get a duplicate instance error if we uncomment...
-- instance Functor (Map k) where
--  fmap f m = fromList $ f $ toList m

-- Since Functor is already defined, we'll make our own typeclass
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance (Ord k) => Functor' (Map k) where
  fmap' f m = fromList (map (\(x, y) -> (x, f y)) (toList m))

fmapped = fmap' (* 19) (fromList [("a", 1), ("b", 2)])

class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
  tofu = Frank
