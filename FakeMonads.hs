module FakeMonads where
  class (Functor m) => Comonad m where
    extract :: m a -> a
    deplicate :: m a -> m (m a)

    {- RULE
      extract . deplicate      === id
      fmap extract . deplicate === id
      deplicate . deplicate === fmap deplicate . deplicate
    -}

    extend :: (m a -> b) -> m a -> m b
    extend f = (fmap f) . deplicate

    infixr 2 =<>
    (=<>) :: (m a -> b) -> m a -> m b
    (=<>) = extend

    infixl 2 <>=
    (<>=) :: m a -> (m a -> b) -> m b
    (<>=) = flip extend

  instance Comonad ((,) a) where
    extract (c, t) = t
    deplicate (c, t) = (c, (c, t))

  instance (Monoid m) => Comonad ((->) m) where
    extract f = f mempty
    deplicate f a b = f $ a `mappend` b