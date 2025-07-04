-- I had to `cabal install --lib random` to be able to import System.Random.

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import System.Random

main = do
  print $ fst (random (mkStdGen 100) :: (Int, StdGen))
  let (tossOne, genOne) = (random (mkStdGen 100) :: (Bool, StdGen))
      (tossTwo, genTwo) = random genOne
      (tossThree, _) = random genTwo
  print [tossOne, tossTwo, tossThree]
