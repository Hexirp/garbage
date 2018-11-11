module Random.Xorshift
 ( xorshift32
 ) where
 import Prelude
 import System.Random
 import Data.Word
 import Data.Bits (Bits, xor, shift)

 xorshift32 :: Word32 -> Word32
 xorshift32 = xs 5 . xs (negate 17) . xs 13
 
 xs :: Bits b => Int -> b -> b
 xs n x = x `xor` (x `shift` n)
