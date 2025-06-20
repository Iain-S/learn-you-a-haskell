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
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]  

ident :: a -> a
ident x = x

-- Constrained with a "class constraint"
ident' :: Num a => a -> a
ident' x = x

-- Guess the type of head
-- takes a list of things and returns a thing
head' :: [a] -> a
head' xs = head xs

-- The type of the == operators is
-- (==) :: Eq a => a -> a -> Bool