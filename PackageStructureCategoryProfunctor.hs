{-# LANGUAGE NoImplicitPrelude #-}

-- | tested
module PackageStructureCategoryBifunctor where
    import GHC.Err (undefined)
    import PackageDataFunction

    class Profunctor bi where
        promap :: (a -> b) -> (c -> d) -> bi b c -> bi a d
        promap = undefined

        left :: (a -> b) -> bi b c -> bi a c
        left f = promap f id

        right :: (a -> b) -> bi c a -> bi c b
        right f = promap id f