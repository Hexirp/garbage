{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
--
-- Module      :  Package.Structure.Swap
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

module Package.Structure.Swap (
    Swap(..),
    And(..),
    Or(..)
) where
    import Package.Data.Function
    import Package.Data.Void
    import Package.Structure.Type

    class {- Dutype s => -} Swap s where
        -- | Swapping.
        -- 
        -- Law:
        --   @swap . swap = id@
        swap :: s a b -> s b a
        swap = undefined

    class Swap and => And and where
        product :: a -> b -> and a b
        product a b = (const a) `foesac` (const b)

        foesac :: (a -> b) -> (a -> c) -> (a -> and b c)
        foesac f g x = f x `product` g x

        first :: and a b -> a
        first = second . swap

        second :: and a b -> b
        second = first . swap

        delta :: a -> and a a
        delta = foesac id id

    class Swap or => Or or where
        caseof :: (a -> c) -> (b -> c) -> (or a b -> c)
        caseof = undefined

        left :: a -> or a b
        left = swap . right

        right :: b -> or a b
        right = swap . left

        alted :: or a a -> a
        alted = caseof id id