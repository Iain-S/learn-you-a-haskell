import Control.Applicative
import Control.Monad (MonadPlus (mzero))

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

-- Applicative vs Monad Challenge from chatGPT
-- give this function...
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

notin = safeDivide 1 0

eight = safeDivide 16 2

-- Use Applicative style to divide 100 by 10, and then add 5.
-- applicative is f a -> f (a->b) -> fb
-- so we want to put Maybes in and expect Maybes out.
app = safeDivide <$> Just 100 <*> Just 10

-- Use Monad style to divide 100 by a value from a Maybe Double,
-- and if the result is greater than 5, subtract 1, otherwise return Nothing.
swap2 f x y = f y x

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

resul = Just 3.4 >>= safeDivide 100 >>= (\x -> guard (x > 5) >> return x) >>= (\x -> Just (x - 1))

mySolution :: Double -> Maybe Double
mySolution x = safeDivide 100 x >>= (\x -> guard (x > 5) >> return x) >>= (\x -> Just (x - 1))

chatSolution :: Double -> Maybe Double
chatSolution x = do
  result <- safeDivide 100 x
  guard (result > 5)
  return (result - 1)

-- ...
