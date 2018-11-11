-- Bint = big + int
type Bint = Integer

-- 引数が適用できる範囲に入っていないときのメッセージ
outOfRngErr = error "Argument is not in the range that can be applied."

-- 負の数がないかチェックする関数
isMns :: [Bint] -> Bool
isMns []          = False
isMns (x : xList) = (x < 0) || isMns xList

-- 関数の適用を繰り返す関数
loop :: Bint -> (a -> a) -> (a -> a)
loop n f | isMns [n] = outOfRngErr
loop 0 f = id
loop n f = f . loop (n - 1) f

-- 階乗
fac :: Bint -> Bint
fac n | isMns [n] = outOfRngErr
fac 0 = 1
fac n = n * fac (n - 1)

-- フィナボッチ数
fib :: Bint -> Bint
fib n | isMns [n] = outOfRngErr
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- ハイパー演算子
hyper :: Bint -> Bint -> Bint -> Bint
hyper n x y | isMns [n,x,y] = outOfRngErr
hyper 0 x y = y + 1
hyper 1 x 0 = x
hyper 2 x 0 = 0
hyper n x 0 = 1
hyper n x y = hyper (n - 1) x $ hyper n x (y - 1)

{- -- hyperの単純＆高速化
hyper' :: Bint -> Bint -> Bint -> Bint
hyper' n x y | isMns [n,x,y] = outOfRngErr
hyper' 0 x y = y + 1
hyper' 1 x y = x + y
hyper' 2 x y = x * y
hyper' 3 x y = x ^ y
hyper' n x 0 = 1
hyper' n x y = loop (x - 1) (hyper' (n - 1) x) $ y -}

{- upArrow :: Bint -> Bint -> Bint -> Bint
upArrow n x y = hyper (n + 2) x y -}

-- アッカーマン関数
ack :: Bint -> Bint -> Bint
ack m n | isMns [m,n] = outOfRngErr
ack 0 n = n + 1
ack m 0 = ack (m - 1) 1
ack m n = ack (m - 1) $ ack m (n - 1)

-- スタインハウス・モーザー表記の関数化
moser :: Bint -> Bint -> Bint
moser n x 
    | isMns [n,x] = outOfRngErr
    | n <= 3      = hyper n x x
    | otherwise   = loop x (moser (n - 1)) x

{- -- コンウェイのチェーン表記の関数化（引数にリストを使っている都合上、引数の並びが逆になっている。）
rvsChain :: [Bint] -> Bint
rvsChain list | isMns $ map (\x -> x - 1) list = outOfRngErr
rvsChain []               = 0
rvsChain (a : [])         = a
rvsChain (a : b : [])     = b ^ a
rvsChain (1 : xList)      = rvsChain xList
rvsChain (a : b : c : []) = upArrow a c b {- 本当は a -> b -> c = upArrow c a b 
    | 逆転して、 c <- b <- a = upArrow c a b
    | 引数の名前を入れ替えて、　a <- b <- c = upArrow a c b -}
rvsChain (a : 1 : xList)  = rvsChain $ xList
rvsChain (a : b : xList)  = rvsChain $ (a - 1) : (rvsChain $ a : (b - 1) : xList) : xList -}

{- -- 引数の正常化
chain :: [Bint] -> Bint
chain = rvsChain . reverse -}

-- 結果の表示用
main = do
    print $ 2334