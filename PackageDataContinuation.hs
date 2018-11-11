{-# LANGUAGE NoImplicitPrelude #-}

-- | tested
module PackageDataContinuation where
    import PackageDataFunction

    newtype Cont r a = Cont { runCont :: (a -> r) -> r }