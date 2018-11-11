module Control.Object.Random.Xorshift
 ( xorshift32
 ) where
 import Prelude
 import Data.Word
 import Control.Object.Stream
 import qualified Random.Xorshift as R

 xorshift32 :: Applicative f => Word32 -> StrObj f Word32
 xorshift32 = iterateObj R.xorshift32
