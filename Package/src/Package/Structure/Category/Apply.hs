{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
--
-- Module      :  Package.Structure.Apply
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

module Package.Structure.Category.Apply (
    Apply(..),
    Bind(..)
) where
    import GHC.Error (undefined)
    import Package.Data.Function
    import Package.Structure.Product (Product(..))
    import Package.Structure.Category.Functor (Functor(..))

    class (Functor f) => Apply f where
        fapp :: f (a -> b) -> f a -> f b
        fapp = lift2 ($)

        (<*>) :: f (a -> b) -> f a -> f b
        a <*> b = fapp a b

        (<*) :: f b -> f a -> f b
        a <* b = const <$> a <*> b

        (*>) :: f a -> f b -> f b
        a *> b = const id <$> a <*> b

        lift :: (a -> b) -> (f a -> f b)
        lift = fmap

        lift2 :: (a -> b -> c) -> (f a -> f b -> f c)
        lift2 f a b = f <$> a <*> b

        lift3 :: (a -> b -> c -> d) -> (f a -> f b -> f c -> f d)
        lift3 f a b c = f <$> a <*> b <*> c

        zip :: (Product and) =>(and a b -> c) -> (and (f a) (f b) -> f c)
        zip = uncurry . lift2 . curry

    infixl 3 <*>, <*, *>

    class (Apply f) => Bind f where
        bind :: (a -> f b) -> f a -> f b
        bind a = join . fmap a

        (=<<) :: (a -> f b) -> f a -> f b
        a =<< b = bind a b

        (-<<) :: f b -> f a -> f b
        a -<< b = const a =<< b

        join :: f (f a) -> f a
        join = bind id

        (<=<) :: (b -> f c) -> (a -> f b) -> (a -> f c)
        f <=< g = bind f . g

    infixl 2 =<<, -<<, <=<
