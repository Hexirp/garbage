{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
--
-- Module      :  Package.Structure.Functor
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

module Package.Structure.Category.Functor (
    Functor(..),
    Contravariant(..),
    Invariant(..),
    Nonvariant(..)
) where
    import Package.Data.Void
    import Package.Data.Function
    import Package.Structure.Type
    import Package.Structure.Swap

    -- | It's "Invariant".
    --
    -- "Invariant" is bottom and __+ & -__.
    --
    --  The law:
    --  @
    --      invar id === id
    --      invar (f . g) === invar f . invar g
    --  @
    class {- (Unitype f) => -} Invariant f where
        invar :: (a -> a) -> f a -> f b
        invar = undefined

    -- | It's "Functor".
    --
    -- "Functor" is covarinat and __+__.
    --
    --  The law:
    --  @
    --      fmap id === id
    --      fmap (f . g) === fmap f . fmap g
    --  @
    class (Invariant f) => Functor f where
        fmap :: (a -> b) -> f a -> f b
        fmap = undefined

        (<$>) :: (a -> b) -> f a -> f b
        a <$> b = fmap a b

        (<$) :: b -> f a -> f b
        a <$ b = const a <$> b

    infixr 4 <$>, <$

    --  Law:@
    --      contra id === id
    --      contra (f . g) === contra g . contra f
    --  @
    class (Invariant f) => Contravariant f where
        contra :: (a -> b) -> f b -> f a
        contra = undefined

    class (Functor f, Contravariant f) => Nonvariant f where
        nonvar :: (And and) => (a -> b) -> and (f a -> f b) (f b -> f a)
        nonvar = fmap `foesac` contra