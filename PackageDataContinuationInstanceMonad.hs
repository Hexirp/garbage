{-# LANGUAGE NoImplicitPrelude #-}

-- | tested
module PackageDataContinuationInstanceMonad where
    import PackageDataContinuation
    import PackageDataFunction
    import PackageDataTuple
    import PackageStructureCategoryFunctor
    import PackageStructureCategoryMonad
    import PackageDataContinuationInstanceFunctor

    instance Wrapper (Cont r) where
        wrap a = Cont $ \r ->
            r a

    instance Pointed (Cont r) where

    instance Applicative (Cont r) where

    instance Monad (Cont r) where