{-# LANGUAGE NoImplicitPrelude #-}

module Cont where
    import Prelude

    type Cont r a = (a -> r) -> r

    callCC :: ((a -> b) -> a) -> (a -> b) -> b
    callCC f k = k (f k)

    fact :: Integer -> Cont r Integer
    fact 0 k = k 1
    fact n k = fact (n - 1) $ k . (n *)

    mapC :: (a -> b) -> [a] -> Cont r [b]
    mapC f [] k = k []
    mapC f (x : xs) k = mapC f xs $ k . (f x :)

    foldC :: (a -> b -> b) -> [] a -> b -> Cont r b
    foldC f [] b k = k b
    foldC f (x : xs) b k = foldC f xs b $ k . (f x)

    lengthC :: [a] -> Cont r Integer
    lengthC [] k = k 0
    lengthC (x : xs) k = lengthC xs $ k . (1 +)

    foldN :: (b -> b) -> Integer -> b -> Cont r b
    foldN f 0 b k = k b
    foldN f n b k = foldN f (n - 1) b $ k . f