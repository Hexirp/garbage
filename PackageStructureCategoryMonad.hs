{-# LANGUAGE NoImplicitPrelude #-}

-- | tested
module PackageStructureCategoryMonad where
    import GHC.Err (undefined, error)
    import PackageDataFunction
    import PackageStructureCategoryFunctor

    class (Singletype f) => Wrapper f where
        wrap :: a -> f a
        wrap = undefined

    class (Wrapper f, Functor f) => Pointed f where
        point :: a -> f a
        point = wrap

    class (Apply f, Pointed f) => Applicative f where
        pure :: a -> f a
        pure = point

    class (Bind f, Applicative f) => Monad f where
        (>>=) :: f a -> (a -> f b) -> f b
        a >>= b = flip bind a b

        (>>-) :: f a -> f b -> f b
        a >>- b = flip (-<<) a b

        return :: a -> f a
        return = pure

        (>=>) :: (a -> f b) -> (b -> f c) -> (a -> f c)
        f >=> g = flip (<=<) f g

    infixr 1 >>=, >>-, >=>

    -- | Test classes
    -- | Extend + Applicative -> Extentive
    class (Extend f, Applicative f) => Extentive f where
        -- undefined