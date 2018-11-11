{-# LANGUAGE EmptyDataDecls, NoImplicitPrelude #-}

module PackageDateVoid (Void,undefined,explaw) where
    -- import Prelude (error)

    data Void

    undefined :: Void
    undefined = undefined

    -- explaw = Explosion law
    explaw :: Void -> a
    explaw = explaw