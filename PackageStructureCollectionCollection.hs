{-# LANGUAGE NoImplicitPrelude #-}

module PackageStructureCollectionCollection where
    import GHC.Err (undefined)
    import PackageDataFunction
    import PackageStructureCategoryFunctor (Singletype)

    class (Singletype c) => Collection c where
        empty :: c a
        empty = undefined

        add :: a -> c a -> c a
        add = undefined

        (<:) :: c a -> a -> c a
        (<:) = flip add

    infixl 5 <: