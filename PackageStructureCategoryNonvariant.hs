{-# LANGUAGE NoImplicitPrelude #-}

module PackageStructureCategoryNonvariant where
    import GHC.Err (undefined)
    import PackageDataFunction
    import PackageStructureCategoryFunctor (Singletype)

    class (Singletype f) => Nonvariant f where
        nonvar :: (a -> b) -> Duple (f a -> f b) (f b -> f a)