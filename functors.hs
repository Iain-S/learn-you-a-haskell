import Control.Applicative

mainPlusPlus = do
  line' <- fmap reverse getLine
  putStrLn $ "you said " ++ line'

-- Applicative functors
data Maybe' a = Just' a | Nothing' deriving (Show)

instance Functor Maybe' where
  -- fmap :: (a -> b) -> a -> b
  fmap f Nothing' = Nothing'
  fmap f (Just' a) = Just' $ f a

instance Applicative Maybe' where
  pure = Just'
  Nothing' <*> _ = Nothing'
  -- Just' f <*> Just' b = Just' $ f b
  -- Note the better implementation below.
  Just' f <*> something = fmap f something

-- So we can do partial application with pures, <*> and <*>
-- ghci> pure (+) <*> Just' 3 <*> Just' 7
-- which is equivalent to
-- ghci> fmap (+) Just' 3 <*> Just' 7
-- and there is the <$> operator, which will let us do this
-- ghci> (+) <$> Just' 3 <*> Just' 7
-- Just' 12

-- And we can apply functions of two params like so
-- ghci> [(*), (+), (/)] <*> [7, 8, 9] <*> [1,2,3]

-- This applicative style
main = (++) <$> getLine <*> getLine

-- is the same as
mainEq = do
  a <- getLine
  b <- getLine
  return $ a ++ b

-- Rather than using <*> with List, which gives
-- a Cartesian product, we can use ZipList to pair
-- up the items in two lists

zipper =
  getZipList $ (+) <$> ZipList [1, 2, 3] <*> ZipList [100, 100, 100]

tupler =
  getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"

-- liftA2 takes a normal, binary, function and promotes it
-- to a function that operates on two functors.
-- For example, we could lift :
lifted = liftA2 (:) (Just' 1) (Just' [2, 3])

-- implement example with fold
sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

-- sequenceA_ = foldr (liftA2 (:)) (pure [])
