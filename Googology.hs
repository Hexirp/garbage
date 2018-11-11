{-# LANGUAGE NoImplicitPrelude #-}

module Googology where
    import Prelude hiding ()
    -- import Test.Hspec.Core.Spec
    -- import Test.Hspec.Expectations

    type Nat = Integer

    loop :: Nat -> (a -> a) -> (a -> a)
    loop n f = case n of
        0 -> id
        x -> (f .) $ loop (x - 1) f

    infixl 0 -:
    (-:) :: a -> (a -> b) -> b
    x -: f = f x

    infixr 9 >>>
    (>>>) :: (a -> b) -> (b -> c) -> (a -> c)
    f >>> g = g . f

    -- 原始帰納関数
    suc :: Nat -> Nat
    suc = (1 +)

    plus :: Nat -> Nat -> Nat
    plus a b = a + b

    mult :: Nat -> Nat -> Nat
    mult a b = a * b

    expo :: Nat -> Nat -> Nat
    expo a 0 = 1
    expo a b = mult a $ expo a (b - 1)

    fact :: Nat -> Nat
    fact 0 = 1
    fact n = mult n $ fact (n - 1)

    tetr :: Nat -> Nat -> Nat
    tetr a 0 = 1
    tetr a b = expo a $ expo a (b - 1)

    exfa :: Nat -> Nat
    exfa 0 = 0
    exfa n = expo a $ exfa (n - 1)

    -- 多重帰納関数
    acka :: Nat -> Nat -> Nat
    acka 0 y = suc y
    acka x 0 = acka (x - 1) 1
    acka x y = acka (x - 1) $ acka x (y - 1)

    hype :: Nat -> Nat -> Nat -> Nat
    hype 1 a b = plus a b
    hype x a 1 = a
    hype x a b = hype (x - 1) a $ hype x a (b - 1)

    stms :: Nat -> Nat -> Nat
    stms 3 b = expo b b
    stms a b = loop b (stms (a - 1)) b

    fishS :: (Nat, Nat -> Nat) -> (Nat, Nat -> Nat)
    fishS (n, f) = (fishG f n, fishG f)
        where
            fishG :: Nat -> Nat
            fishG n = fishB n n
            fishB :: Nat -> Nat -> Nat
            fishB 0 n = f n
            fishB m 0 = fishB (m - 1) 1
            fishB m n = fishB (m - 1) $ fishB m (n - 1)

    -- hypeの研究
    hypeC :: Nat -> Nat -> Nat -> (Nat -> r) -> r
    hypeC 1 a b ret = plus a b          $ ret
    hypeC _ a 1 ret = a                 $ ret
    hypeC x a b ret = hypeC x a (b - 1) $ hypeC (x - 1) a >>> ret

    hypeStack :: Nat -> [(Nat, Nat)] -> Nat
    hypeStack a   []             = a
    hypeStack a   ((1, y) : bs)  = hypeStack (a + y) $ bs
    hypeStack 1   ((x, y) : bs)  = hypeStack (y    ) $ bs
    hypeStack a b@((x, y) : _ )  = hypeStack (a - 1) $ (x, y) : (x - 1, y) : b

    -- | The extended hyper operator
    hyper 
      :: Nat -- ^ The base number
      -> [[Nat]] -- ^ The number's array
      -> Nat -- ^ The value
      -> [String] -- ^ The log
      -> ([String], Nat) -- ^ The output
    hyper base array value log 
      | grd base array value = hyp array value
        let 
          -- | [a]=a
          hyp [] a = (log', a)
          -- | ...()[a]=[a+x]
          hyp ([]:xs) a = hyper base xs (a+base) log'
          {-| 
            ...((1),,,)[a]=...(,,,)[a]
            ...(,,,)[1]=...[x]
            ...(,,,(y)###)[a]=...(,,,(y-1)%%%)[a]
          -}
          hyp ((1:ys):xs) a = case cut (1:ys) (a+base) of 
            [] -> hyper base xs a log'
            z -> if a == 1 
              then hyper base xs a
          -- | ...(,,,(y))[a]=...(,,,(y-1))(,,,(y))[a-1]
          hyp ((y:ys):xs) a = hyper base ((y:ys) : ((y-1):ys) : xs) (a-1) log'
          -- | The next
           log
          log' = log

    -- type Bug = [Nat]

    -- bekl :: Nat -> Nat
    -- bekl n = worm ([n], 0)
    --     where
    --         worm :: (Bug, Nat) -> Nat
    --         worm x = case cedric x of
    --             ([], m) -> m
    --             (xs, m) -> worm (xs, m)
    --         cedric :: (Bug, Nat) -> (Bug, Nat)
    --         cedric ([], m) = ([], m)
    --         cedric (0 : xs, m) = (xs, m + 1)

    -- テスト

    -- main :: IO ()
    -- main = hspec spec

    -- spec :: Spec
    -- spec = describe "Check primitive recursive functions" $ do
    --     it "is a computation of suc" $
    --         suc (ton 0) `shouldBe` ton 2
    --     it "is a computation of plus" $
    --         plus (ton 2) (ton 3) `shouldBe` ton 5
    --     it "is a computation of mult" $
    --         mult (ton 3) (ton 2) `shouldBe` ton 6