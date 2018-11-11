{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
--
-- Module      :  Package.Structure.Type
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

module Package.Structure.Type (
    Type,
    Nullitype,
    Unitype,
    Dutype,
    Tritype
) where

    -- | It's all types.
    class Type t where

    -- | It's a type has no argument.
    class (Type t) => Nullitype (t :: *) where

    -- | It's a type has a argument.
    class (Type t) => Unitype (t :: * -> *) where

    -- | It's a type has the two arguments.
    class (Type t) => Dutype (t :: * -> * -> *) where

    -- | It's a type has the three arguments.
    class (Type t) => Tritype (t :: * -> * -> * -> *) where
