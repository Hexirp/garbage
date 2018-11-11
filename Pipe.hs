{-# LANGUAGE GADTs #-}

module Example.Pipe where
 import Prelude

 data Pipe i u o r = Done r | Yeild o (Pipe i u o r) | Await (i -> Pipe i u o r) (u -> Pipe i u o r)

 (.|) :: Pipe i u o r -> Pipe o r i' u' -> Pipe i u i' u'
 p .| Done r = Done r
 p .| Yeild o q = Yeild o (p .| q)
 p .| Await h i = case p of
  Done r -> i r
  Yeild o p' -> p' .| h o
  Await h i -> Await
   (\o -> i o)
   (\u -> h u)
