{-# LANGUAGE NoImplicitPrelude #-}

-- | tested
module PackageStructureCategoryComonad where
    import GHC.Err (undefined, error)
    import Data.String (String)
    import PackageDataFunction
    import PackageStructureCategoryFunctor

    class (Singletype f) => Unwrapper f where
        unwrap :: f a -> a
        unwrap = undefined

    class (Unwrapper f, Functor f) => Copointed f where
        copoint :: f a -> a
        copoint = unwrap

    class (Coapply f, Copointed f) => Extract f where
        extract :: f a -> a
        extract = copoint

    class (Extend f, Extract f) => Comonad f where
        (=>>) :: f a -> (f a -> b) -> f b
        a =>> b = flip extend a b

        (->>) :: f a -> b -> f b
        a ->> b = flip (<<-) a b

        (=>=) :: (f a -> b) -> (f b -> c) -> (f a -> c)
        f =>= g = flip (=<=) f g

    infixr 1 =>>, ->>, =>=

    -- | Test classes
    -- | Bind + Extract -> Bindact
    class (Bind f, Extract f) => Bindact f where
        -- undefined