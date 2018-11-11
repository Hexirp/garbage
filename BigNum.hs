module BigNum where
    import Test.QuickCheck
    import Debug.Trace

    -- 補助関数
    loop :: Integer -> (a -> a) -> a -> a
    loop a f = case a of
        0 -> id
        a -> loop (a - 1) f . f

    first :: (a, b) -> a
    first (a, b) = a

    second :: (a, b) -> b
    second (a, b) = b

    -- 基礎関数
    hyper :: Integer -> Integer -> Integer -> Integer
    hyper a x y = case a of
        1 -> x + y
        a -> case y of
            2 -> hyper (a - 1) x x
            y -> hyper (a - 1) x $ hyper a x (y - 1)

    hyper' :: Integer -> Integer -> Integer -> Integer
    hyper' a x y = case a of
        1 -> x + y
        a -> loop (y - 1) (hyper (a - 1) x) x

    ack :: Integer -> Integer -> Integer
    ack x y = case x of
        0 -> y + 1
        x -> case y of
            0 -> ack (x - 1) 1
            y -> ack (x - 1) $ ack x (y - 1)

    ack' :: Integer -> Integer -> Integer
    ack' x y = case x of
        1 -> y + 2
        x -> loop y (ack (x - 1)) 2

    -- ふぃっしゅ氏による関数(バージョン1)
    fishS :: (Integer, Integer -> Integer) -> (Integer, Integer -> Integer)
    fishS (a, f) = (fishG f a, fishG f)

    fishB :: (Integer -> Integer) -> Integer -> Integer -> Integer
    fishB f a b = case a of
        0 -> f b
        a -> case b of
            0 -> fishB f (a - 1) 1
            b -> fishB f (a - 1) $ fishB f a (b - 1)

    fishG :: (Integer -> Integer) -> Integer -> Integer
    fishG f a = fishB f a a

    -- コンウェイのチェーン表記
    data Chain = O | Chain :++ Integer deriving (Show)
    infixl 1 :++

    chain :: Chain -> Integer
    chain (O)             = chain $ O :++ 1 :++ 1
    chain (O :++ a)       = chain $ O :++ a :++ 1
    chain (O :++ a :++ b) = hyper 3 a b
    chain (x :++ 1)       = chain $ x
    chain (x :++ 1 :++ b) = chain $ x
    chain (x :++ a :++ b) = chain $ x :++ chain (x :++ (a - 1) :++ b) :++ (b - 1)

    -- 順序数
    -- data Ordinal = Zero | Suc Ordinal | Omega | Hyper Ordinal Ordinal Ordinal deriving (Eq, Show)

    -- plus :: Ordinal -> Ordinal -> Ordinal
    -- plus Zero a = a
    -- plus a Zero = a
    -- plus (Suc a) b = Suc (plus a b)
    -- plus a (Suc b) = Suc (plus a b)
    -- plus Omega a = 

    {- quickSort :: (Show a, Ord a) => [a] -> [a]
    quickSort [] = []
    quickSort xs = trace ("\n*****" ++ show xs) $ qsort $ choice (xs, [])

    choice :: (Show a, Ord a) => ([a], [a]) -> (a, [a])
    choice (x : xs, i) = case xs of
        [] -> (x, [])
        (y : ys) -> case ys of
            [] -> (x, y : i)
            (z : zs) -> case zs of
                [] -> case (x < y) of
                    False -> case (x < z) of
                        False -> (z, x : y : i)
                        True -> (y, z : x : i)
                    True -> case (y < z) of
                        False -> (x, y : z : i)
                        True -> (y, x : z : i)
                ws -> choice $ up (choice ([x,y,z], i)) `contact` up (choice (ws, []))
        where
            contact (a, xs) (b, ys) = (b ++ a, xs ++ ys)
            up (a, xs) = ([a], xs)

    qsort :: (Show a, Ord a) => (a, [a]) -> [a]
    qsort (a, xs) = pair a $ qSort a xs

    pair :: a -> ([a], [a]) -> [a]
    pair x (a, b) = a ++ [x] ++ b

    qSort :: (Show a, Ord a) => a -> [a] -> ([a], [a])
    qSort a xs = qs a xs ([], [])

    qs :: (Show a, Ord a) => a -> [a] -> ([a], [a]) -> ([a], [a])
    qs a [] (ys, zs) = trace ("\n+++++" ++ show a ++ " " ++ show (ys, zs)) $ (quickSort ys, quickSort zs)
    qs a (x : xs) (ys, zs) = case x < a of
        False -> qs a xs (ys, x : zs)
        True -> qs a xs (x : ys, zs)

    sorted :: (Show a, Ord a)=> [a] -> Bool
    sorted [] = True
    sorted (x : []) = True
    sorted (x : y : zs) = case x <= y of
        False -> False
        True -> sorted (y : zs)

    -- quickSort [1]
    -- qsort (choice [1]) [1]
    -- qsort 1 [1]
    -- pair $ qSort 1 [1]
    -- pair $ qs 1 [1] ([], [])
    -- pair $ qs 1 [] ([1], [])
    -- pair $ (quickSort [1], quickSort [])
    -- quickSort [1] ++ []

    force :: (Show a) => a -> ()
    force a = const () $! a

    prop_sorted :: (Show a, Ord a) => [a] -> Bool
    prop_sorted xs = sorted $ quickSort xs -}

