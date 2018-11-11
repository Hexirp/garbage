{-# LANGUAGE NoImplicitPrelude #-}

-- | tested
module PackageStructureCategoryBifunctor where
    import GHC.Err (undefined)
    import PackageDataFunction
    import PackageDataTuple

    class Bifunctor bi where
        bimap :: (a -> b) -> (c -> d) -> bi a c -> bi b d
        bimap = undefined

        first :: (a -> b) -> bi a c -> bi b c
        first f = bimap f id

        second :: (a -> b) -> bi c a -> bi c b
        second f = bimap id f

    class (Bifunctor bi) => Biapply bi where
        biapp :: bi (a -> b) (c -> d) -> bi a c -> bi b d
        biapp = bift2 ($) ($)

        bift2 :: (a -> b -> c) -> (d -> e -> f) -> bi a d -> bi b e -> bi c f
        bift2 f g a b = bimap f g a `biapp` b

        bizip :: (Duple a b -> c) -> (Duple d e -> f) -> Duple (bi a d) (bi b e) -> bi c f
        bizip = \f -> \g -> uncurry (bift2 (curry f) (curry g))