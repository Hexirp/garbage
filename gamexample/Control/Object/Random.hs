module Control.Object.Random
 ( rand
 , randR
 ) where
 import Prelude
 import System.Random
 import Control.Object.Stream

 rand :: (Applicative f, Random r, RandomGen g) => g -> StrObj f r
 rand = pureStrObj random

 randR :: (Applicative f, Random r, RandomGen g) => (r, r) -> g -> StrObj f r
 randR = curry $ pureStrObj rr where
  rr :: (Random r, RandomGen g) => ((r, r), g) -> (r, ((r, r), g))
  rr (ran, g) = let (r, g) = randomR ran g in (r, (ran, g))
