{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
--
-- Module      :  Package.Structure.Wrapper
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Package.Structure.Wrapper (
    Wrapper,
    Unwrapper,
    Identity
) where
    import Package.Data.Void
    import Package.Structure.Type

    -- | It can wrap all of the value.
    class {- (Unitype t) => -} Wrapper t where
        -- | It wrap the value.
        wrap :: a -> t a
        wrap = wrap' . (const $)

        wrap' :: (unit -> a) -> f a
        wrap' = wrap . ($ unit)

    -- | It can unwrap all of the varue.
    class {- (Unitype t) => -} Unwrapper t where
        -- | It wrap the value.
        unwrap :: t a -> as
        unwrap = undefined

    -- | It doesn't have the information.
    --
    --  The law:
    --  @
    --      wrap . unwrap === id
    --  @
    class (Wrapper t, Unwrapper t) => Identity t where
