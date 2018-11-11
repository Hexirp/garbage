{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances, IncoherentInstances #-}

module PackageStructureCategoryIdentity where
    import PackageDataTuple
    import PackageDataFunction
    import PackageStructureCategoryFunctor
    import PackageStructureCategoryMonad
    import PackageStructureCategoryComonad

    class (Wrapper f, Unwrapper f) => Identity f where
        {- Law: wrap . unwrap = unwrap . wrap = id -}

    instance Identity f => Singletype f where
        singletype = wrap Unit

    instance Identity f => Wrapper f where
        wrap = wrap

    instance Identity f => Unwrapper f where
        unwrap = unwrap

    instance Identity f => Functor f where
        fmap f = wrap . f . unwrap

    instance Identity f => Apply f where
        fapp f = wrap . unwrap f . unwrap

    instance Identity f => Bind f where
        bind f = f . unwrap
    
    instance Identity f => Extend f where
        extend f = wrap . f

    instance Identity f => Pointed f where
        point = wrap

    instance Identity f => Copointed f where
        copoint = unwrap

    instance Identity f => Applicative f where

    instance Identity f => Extract f where

    instance Identity f => Monad f where

    instance Identity f => Extentive f where

    instance Identity f => Comonad f where

    instance Identity f => Bindact f where