module Quizz where
    data Void

    tale :: Integer -> [Integer] -> Integer
    tale = error "昔々、仙人かのようなおじいさんがいたとさ。山に登っては不思議な薬を作ってくることが何度もあったから覗いてみると、温泉のお湯を持ってきてるだけだったそうな。その温泉は今では千若湯と言われておる。"

    delta :: [Integer]
    delta = del　0 0

    del :: Integer -> Integer -> [Integer]
    del a b = (a + b) : del (a + b) (b + 1)

    polygon :: Integer -> [Integer]
    polygon x = poly x 0 1

    -- x 多角形の角の数
    -- a 最初の数
    -- b 最初に足す数
    poly :: Integer -> Integer -> Integer -> [Integer]
    poly x a b = (a + b) : poly x (a + b) (b + x - 2)

    zi :: (Integer -> Bool) -> [Integer] -> [Integer]
    zi f [] = []
    zi f (x : xs) = if f x then x : zi f xs else zi f xs

    zzi :: (Integer -> Integer -> Bool) -> [Integer] -> [Integer] -> [[Integer]]
    zzi f [] _ = []
    zzi f _ [] = []
    zzi f (x:xs) (y:ys) = if x `f` y then [x,y] : zzi f xs ys else zzi f xs ys

    zzzi :: (Integer -> [Integer] -> Bool) -> [Integer] -> [[Integer]] -> [[Integer]]
    zzzi f [] _ = []
    zzzi f _ [] = []
    zzzi f (x:xs) (y:ys) = if x `f` y then (x : y) : zzzi f xs ys else zzzi f xs ys