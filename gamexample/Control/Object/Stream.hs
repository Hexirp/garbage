{-# LANGUAGE RankNTypes #-}

module Control.Object.Stream
 ( StrObj
 , iterateObj
 , pureStrObj
 , genStrObj
 ) where
 import Prelude
 import Control.Object

 type StrObj m a = Object ((->) a) m

 iterateObj :: Applicative f => (s -> s) -> s -> StrObj f s
 iterateObj f s = Object $ \k -> let s' = f s in pure (k s', iterateObj f s')

 pureStrObj :: Applicative f => (s -> (a, s)) -> s -> StrObj f a
 pureStrObj f s = Object $ \k -> let (a, s') = f s in pure (k a, pureStrObj f s')

 genStrObj :: Monad m => (s -> m (a, s)) -> s -> StrObj m a
 genStrObj f s = Object $ \k -> do
  (a, s') <- f s
  return $ (k a, genStrObj f s')
