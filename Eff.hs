{-# LANGUAGE LiberalTypeSynonyms, RankNTypes, FlexibleContexts #-}

module Example.Eff where
 import Prelude (undefined, Int, String, IO, return)
 import Control.Eff
 import Control.Eff.State.Lazy
 import Control.Eff.Lift
 import Control.Eff.Exception

 type Mine r a =
  ( Member (State Int) r
  , Member Fail r
  , SetMember Lift (Lift IO) r
  ) => Eff r a

 context :: (Member (State String) r) => Mine r ()
 context = do
  x <- helper
  return x

 helper :: (Member (State Int) r, Member Fail r) => Eff r ()
 helper = return ()
