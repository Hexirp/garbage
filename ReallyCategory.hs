{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FunctionalDependencies #-}

module ReallyCategory where
    import GHC.Err (undefined)

    type Hask = (->)

    class Category cat where
        id :: cat a a
        id = undefined

        (.) :: cat b c -> cat a b -> cat a c
        (.) = undefined

    class (Category c, Category d) => Functor f c d | f c -> d, f d -> c where
        fmap :: c a b -> d (f a) (f b)
        fmap = undefined

    class (Functor f c d, Functor g d c) => Adjunction f g c d | f -> g c d, g -> f c d where
        phiLeft :: c (f x) y -> d y (g x)
        phiLeft = undefined

        phiRight :: d y (g x) -> c (f x) y
        phiRight = undefined

        -- phiRight . phiLeft  . phiRight = phiRight
        -- phiLeft  . phiRight . phiLeft  = phiLeft

    newtype Compose f g a = Compose { getCompose :: g (f a) }

    newtype Writer s a = Writer (s, a)
    newtype Reader s a = Reader (s -> a)

    instance Category (->) where
        id = \x -> x
        (.) = \f -> \g -> \x -> f (g x)

    instance Functor (Writer s) (->) (->) where
        fmap f = \(Writer (s, a)) -> Writer (s, f a)

    instance Functor (Reader s) (->) (->) where
        fmap f = \(Reader g) -> Reader (\x -> f (g x))

    -- instance (Category c, Category d, Functor f c d, Functor g d c, Adjunction f g c d) => Monad (Conpose f g) where