{-# LANGUAGE NoImplicitPrelude #-}

-- | tested
module PackageStructureCategoryCategory where
    import GHC.Err(undefined)
    import PackageDataTuple

    class Semigroupoid hom where
        (>>>) :: hom a b -> hom b c -> hom a c
        f >>> g = g <<< f

        (<<<) :: hom b c -> hom a b -> hom a c
        f <<< g = g >>> f

        -- | LAW:
        -- | a >>> (b >>> c) === (a >>> b) >>> c

    infixl 1 >>>
    infixr 1 <<<

    class (Semigroupoid hom) => Category hom where
        id :: hom a a
        id = undefined

        -- | LAW:
        -- | id >>> a === a >>> id === a

    class (Semigroupoid hom) => Lift hom where
        arr :: (a -> b) -> hom a b
        arr = undefined

        -- | Law:
        -- | arr id === id
        -- | arr (f >>> g) === arr f >>> arr g

    class (Semigroupoid hom) => Parallel hom where
        (***) :: hom a b -> hom c d -> hom (Duple a c) (Duple b d)
        (***) = undefined

    infix 2 ***

    class (Category hom, Lift hom, Parallel hom) => Arrow hom where
        first :: hom a b -> hom (Duple a x) (Duple b x)
        first f = f *** id

        second :: hom a b -> hom (Duple x a) (Duple x b)
        second f = id *** f

        (&&&) :: hom a b -> hom a c -> hom a (Duple b c)
        f &&& g = arr delta >>> f *** g
            where delta x = Duple x x