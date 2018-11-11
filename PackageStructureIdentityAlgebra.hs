{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances, IncoherentInstances #-}

module PackageStructureIdentityAlgebra where
    import PackageDataFunction
    import PackageStructureAlgebraMagma
    import PackageStructureCategoryFunctor
    import PackageStructureIdentity
    import PackageStructureIdentityCategory

    instance (Identity i, Magma m) => Magma (i m) where
        (<>) = lift2 (<>)