{-# LANGUAGE NoImplicitPrelude #-}

module PackageDataTuple where
    import PackageDataFunction

    data Unit = Unit

    data Duple a b = Duple a b

    curry :: (Duple a b -> c) -> (a -> b -> c)
    curry f a b = f $ Duple a b

    uncurry :: (a -> b -> c) -> (Duple a b -> c)
    uncurry f (Duple a b) = f a b

    data Either a b = Left a | Right b