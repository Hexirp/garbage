{-# LANGUAGE NoImplicitPrelude #-}

-- | tested
module PackageStructureCategoryFunctor where
    import GHC.Err (undefined)
    import PackageDataFunction
    import PackageDataTuple

    class Singletype f where
        singletype :: f Unit
        singletype = undefined

    class (Singletype f) => Functor f where
        fmap :: (a -> b) -> f a -> f b
        fmap = undefined

        (<$>) :: (a -> b) -> f a -> f b
        a <$> b = fmap a b

        (<$) :: b -> f a -> f b
        a <$ b = const a <$> b

    infixr 4 <$>, <$

    class (Functor f) => Apply f where
        fapp :: f (a -> b) -> f a -> f b
        fapp = lift2 ($)

        (<*>) :: f (a -> b) -> f a -> f b
        a <*> b = fapp a b

        (<*) :: f b -> f a -> f b
        a <* b = const <$> a <*> b

        (*>) :: f a -> f b -> f b
        a *> b = const id <$> a <*> b

        lift :: (a -> b) -> (f a -> f b)
        lift = fmap

        lift2 :: (a -> b -> c) -> (f a -> f b -> f c)
        lift2 f a b = f <$> a <*> b

        lift3 :: (a -> b -> c -> d) -> (f a -> f b -> f c -> f d)
        lift3 f a b c = f <$> a <*> b <*> c

        zip :: (Duple a b -> c) -> (Duple (f a) (f b) -> f c)
        zip = uncurry . lift2 . curry

    infixl 3 <*>, <*, *>

    class (Functor f) => Coapply f where
        cozip :: (Either a b -> c) -> (Either (f a) (f b) -> f c)
        cozip = undefined

    class (Apply f) => Bind f where
        bind :: (a -> f b) -> f a -> f b
        bind a = join . (fmap a)

        (=<<) :: (a -> f b) -> f a -> f b
        a =<< b = bind a b

        (-<<) :: f b -> f a -> f b
        a -<< b = const a =<< b
    
        join :: f (f a) -> f a
        join = bind id

        (<=<) :: (b -> f c) -> (a -> f b) -> (a -> f c)
        f <=< g = bind f . g

    infixl 2 =<<, -<<, <=<

    class (Coapply f) => Extend f where
        extend :: (f a -> b) -> f a -> f b
        extend a = (fmap a) . duplicate 

        (<<=) :: (f a -> b) -> f a -> f b
        a <<= b = extend a b

        (<<-) :: b -> f a -> f b
        a <<- b = const a <<= b

        duplicate :: f a -> f (f a)
        duplicate = extend id

        (=<=) :: (f b -> c) -> (f a -> b) -> (f a -> c)
        f =<= g = f . extend g

    infixl 2 <<=, <<-, =<=