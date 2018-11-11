{-# LANGUAGE NoImplicitPrelude #-}

module PackageStructureAlgebraAnotherMagma where
    import GHC.Err (undefined)

    class Magma' m where
        op' :: m -> m -> m
        op' = undefined

        (><) :: m -> m -> m
        a >< b = op' a b

    infixl 6 ><

    class (Magma' m) => SemiGroup' m where
        {- Law: a >< (b >< c) == (a >< b) >< c -}

        (*) :: m -> m -> m
        a * b = op' a b

    class (Magma' m) => EmptyGroup' m where
        {- Law: empty' >< m == m >< empty' == m -}

        empty' :: m
        empty' = undefined

    class (Magma' m) => AbelianMagma' m where
        {- Law: a >< b = b >< a -}

    class (Magma' m) => QuasiGroup' m where
        {- Law: a >< b == c <==> a / c == b -}

        (/) :: m -> m -> m
        a / b = undefined

    class (SemiGroup' m, EmptyGroup' m) => Monoid' m where
        {- Law: 1 & 2 -}

    class (QuasiGroup' m, EmptyGroup' m) => Loop' m where
        {- Law: 1 & 2 -}

        netate' :: m -> m
        netate' a = empty' / a

    class (QuasiGroup' m, EmptyGroup' m, SemiGroup' m) => Group' m where
        {- Law: 1 & 2 & 3 -}

    class (QuasiGroup' m, EmptyGroup' m, SemiGroup' m, AbelianMagma' m) => AbelianGroup' m where
        {- Law: 1 & 2 & 3 & 4 -}