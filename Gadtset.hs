{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Gadtset where
 import Prelude
 import Data.Void

 -- Logic
 
 type Not a = a -> Void

 type And a b = (a, b)

 type Or a b = Not (Not a, Not b)

 type Then a b = a -> b

 type Iff a b = And (Then a b) (Then b a)

 type N a = Not (Not a)

 -- Set
 
 data Set e = Empty | Added e (Set e)

 data Mem e s where
  MemB :: Mem a (Added a s)
  MemR :: Mem a s -> Mem a (Added x s)
 
 type NMem e s = N (Mem e s)

 type (:>) e s = Added e s

 -- Predicate

 data Equ s t where
  Equ :: (forall x. Iff (NMem x s) (NMem x t)) -> Equ s t

 data Sub s t where
  Sub :: (forall x. Then (NMem x s) (NMem x t)) -> Sub s t
 
 data Uni s t u where
  Uni :: (forall x. Iff (Or (NMem x s) (NMem x t)) (NMem x u)) -> Uni s t u

 -- Interpretion
 
 data Product s where
  PEmpty :: Product Empty
  PAdded :: a -> Product s -> Product (a :> s)
 
 -- Function
 
 scurry :: Uni s t u -> (Product u -> r) -> Product s -> Product t -> r
 scurry uni f x y = f (smerge uni x y)

 smerge :: Uni s t u -> Product s -> Product t -> Product u
 smerge = undefined
