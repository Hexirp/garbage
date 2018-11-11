{-# LANGUAGE NoImplicitPrelude #-}

-- | tested
module PackageDataFunction where
    id :: a -> a
    id a = a

    const :: a -> b -> a
    const a b = a

    fix :: (a -> a) -> a
    fix f = f (fix f)

    -- con <- contraction
    con :: (a -> a -> b) -> a -> b
    con f x = f x x

    flip :: (a -> b -> c) -> b -> a -> c
    flip f x y = f y x

    ($) :: (a -> b) -> a -> b
    ($) f x = f x

    infixr 0 $

    (.) :: (b -> c) -> (a -> b) -> a -> c
    (.) f g x = f $ g x

    infixl 9 .