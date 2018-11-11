class Category cat where 
  id :: cat a a 
  (.) :: cat b c -> cat a b -> cat a c 

class (Category cat, Category dat) => Functor fun cat dat | fun cat -> dat, fun cat -> dat where
  fmap :: all a. b. cat a b -> dat (fun a) (fun b)

class (Functor fun dat cat, Functor gun cat dat) => Adjunction fun gun cat dat | fun -> gun, gun -> fun, fun cat -> dat, fun dat -> cat, gun cat -> dat, gun dat -> cat where
  phiLeft  :: all a. b. cat (fun a) b -> dat a (gun b)
  phiRight :: all a. b. dat a (gun b) -> cat (fun a) b

  -- phiRight . phiLeft  . phiRight = phiRight
  -- phiLeft  . phiRight . phiLeft  = phiLeft

newtype Hask a b = Hask (a -> b)

instance Category Hask where
  id = Hask (\x -> x)
  (.) (Hask f) (Hask g) = Hask (\x -> f $ g x)

newtype Writer s a = Writer (s, a)

instance Functor (Writer s) Hask Hask where
  fmap (Hask f) = Hask $ \(Writer (s, x))-> Writer (s, (f x))

newtype  Reader s a = Reader (s -> a)

instance Functor (Reader s) Hask Hask where
  fmap (Hask f) = Hask $ \(Reader g) -> Reader (f . g)

instance Adjunction (Writer s) (Reader s) Hask Hask where
  phiLeft  (Hask f) = Hask $ \x-> Reader $ \s-> f $ Writer (s, x)
  phiRight (Hask g) = \(Writer (s, x))-> let Reader r = g x in Hask $ r s