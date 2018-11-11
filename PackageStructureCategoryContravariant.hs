{-# LANGUAGE NoImplicitPrelude #-}

-- | tested
module PackageStructureCategoryContravariant where
    import GHC.Err (undefined)
    import PackageDataFunction
    import PackageDataTuple
    import PackageStructureCategoryFunctor (Singletype)

    class (Singletype f) => Contravariant f where
        contra :: (a -> b) -> f b -> f a
        contra = undefined

    class (Contravariant f) => Divide f where
        divide :: (Duple a b -> c) -> f c -> Duple (f a) (f b)
        divide = undefined