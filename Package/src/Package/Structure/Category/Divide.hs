{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
--
-- Module      :  Package.Structure.Category.Divide
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

module Divide where
    import Package.Data.Void
    import Package.Data.Function
    import Package.Structure.Type
    import Package.Structure.Swap

    class Conquer c where
        conquer :: f a
        conquer = conquer' $ const unit

        conquer' :: (a -> unit) -> f a
        conquer' = const conquer

    class (Conquer c) => Divide where
        divide :: ()