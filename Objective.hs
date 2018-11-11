{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

xor :: Bool -> Bool -> Bool
xor False False = False
xor True True = False
xor _ _ = True

infixr 5 :+
data Stream a = a :+ (Stream a)

take_stream :: Integer -> Stream a -> [a]
take_stream 0 _ = []
take_stream n (x :+ s) = x : take_stream (n - 1) s

fib :: Stream Integer
fib = 0 :+ 1 :+ series_zip_2 (+) fib

series_zip_2 :: (a -> a -> b) -> Stream a -> Stream b
series_zip_2 f (x :+ y :+ s) = f x y :+ series_zip_2 f (y :+ s)

rand_lfsr_3 :: Stream Bool
rand_lfsr_3 = False :+ False :+ True :+ series_zip_3 lfsr_3 rand_lfsr_3
    where lfsr_3 x y _ = x `xor` y

series_zip_3 :: (a -> a -> a -> b) -> Stream a -> Stream b
series_zip_3 f (x :+ y :+ z :+ s) = f x y z :+ series_zip_3 f (y :+ z :+ s)

map_stream :: (a -> b) -> Stream a -> Stream b
map_stream f (x :+ s) = f x :+ map_stream f s

main :: IO ()
main = main_pr 20

main_pr :: Integer -> IO ()
main_pr n = do
    print $ take_stream n fib
    print $ take_stream n $ map_stream toIntFromBool rand_lfsr_3
    where
        toIntFromBool False = 0
        toIntFromBool True = 1

newtype Object f g = Object { runObject :: forall x. f x -> g (x, Object f g) }

data Counter a where
  Increment :: Counter ()
  Print :: Counter Int

counter :: Int -> Object Counter IO
counter n = Object $ \x -> case x of
  Increment -> return ((), counter (n + 1))
  Print -> print n >> return (n, counter n)