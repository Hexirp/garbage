{-# LANGUAGE NoImplicitPrelude #-}

module PackageStructureCategoryInvariant where
    import GHC.Err (undefined)
    import PackageDataFunction
    import PackageStructureCategoryFunctor (Singletype)

    class (Singletype f) => Invariant f where
        invar :: (a -> a) -> f a -> f a