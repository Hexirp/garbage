{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
--
-- Module      :  Package.Data.Function
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Package.Data.Function (
    id,
    const,
    fix,
    con,
    flip,
    ($),
    (.),
    on
) where
    -- | It's identity function.
    id :: a -> a
    id a = a

    -- | It cut second argment.
    const :: a -> b -> a
    const a b = a

    -- | It is a fixed-point.
    fix :: (a -> a) -> a
    fix f = f (fix f)

    -- | It copy argment.
    --
    --  Name: con <- contraction
    con :: (a -> a -> b) -> a -> b
    con f x = f x x

    -- | It flip two argment.
    flip :: (a -> b -> c) -> b -> a -> c
    flip f x y = f y x

    -- | It's one of sugar syntax.
    ($) :: (a -> b) -> a -> b
    ($) = id

    infixr 0 $

    -- | It combine function.
    (.) :: (b -> c) -> (a -> b) -> a -> c
    (.) f g x = f $ g x

    infixl 9 .

    -- | It apply function to argment.
    on :: (a -> a -> b) -> (c -> a) -> (c -> c -> b)
    on f g x y = g x `f` g x

    infix 9 `on`
