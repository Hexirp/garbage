module NutNum where
	import ListFunc
	import Debug.Trace

	-- NutNum = Natural Number　= 自然数
	data NutNum = Zero | Suc NutNum deriving (Read, Show)

	-- |-| = difference = 「違い」。つまりは \x y -> abs $ x - y と同じ意味。これをオリジナル化した。
	infixl 7 |-|
	(|-|) :: NutNum -> NutNum -> NutNum
	x |-| y = case x of 
		Zero -> y
		Suc a -> case y of
			Zero -> x
			Suc b -> a |-| b

	-- + のオリジナル化。
	infixl 6 !+
	(!+) :: NutNum -> NutNum -> NutNum
	x !+ y = case y of
		Zero -> x
		Suc a -> succ $ x !+ a

	-- 乗算。
	infixl 7 !++
	(!++) :: NutNum -> NutNum -> NutNum
	x !++ y = case y of
		Zero -> Zero
		Suc a -> (x !++ a) !+ x

	--　指数。
	infixr 8 !+++
	(!+++) :: NutNum -> NutNum -> NutNum
	x !+++ y = case y of
		Zero -> Suc Zero
		Suc Zero -> x
		Suc a -> x !++ (x !+++ a)

	-- 減算。
	infixl 6 !-
	(!-) :: NutNum -> NutNum -> NutNum
	x !- y = case y of
		Zero -> x
		Suc a -> pred $ x !- a

	instance Eq NutNum where 
		x == y = 
			case x of 
				Zero -> case y of 
					Zero -> True
					Suc b -> False
				Suc a -> case y of
					Zero -> False
					Suc b -> a == b

	{- 	compare x y 
			| x == y    = EQ
			| x <= y    = LT
			| otherwise = GT -}
	-- compare は2つの引数を取り、それを比較した結果を返す関数
	instance Ord NutNum where
		compare x y = case x of 
			Zero -> case y of 
				Zero -> EQ
				Suc b -> LT
			Suc a -> case y of 
				Zero -> GT
				Suc b -> compare a b

	{- 
	-- a はインスタンスとなった型に変えて考える。この場合ではNutNumにする。
	succ :: a -> a -- 引数の「次」を返す関数。succそのもの。
	pred :: a -> a -- 引数の「前」を返す関数
	toEnum :: Int -> a -- 数からある型aへ変換する関数
	fromEnum :: a -> Int -- ある型aから数へ変換する関数
	enumFrom       :: a -> [a]            -- [n..]     -- a,a+1,a+2... という無限リストを返す関数
	enumFromThen   :: a -> a -> [a]       -- [n,m..]   -- a,b,c... という無限リストを返す関数。 たとえば、 3,5,7,9... 。つまり、等差数列を生成する。
	enumFromTo     :: a -> a -> [a]       -- [n..m]    -- a,b...c,d というリストを返す関数。　3,4...8,9 など。
	enumFromThenTo :: a -> a -> a -> [a]  -- [n,n'..m] -- a,b...c というリストを返す関数。　enumFromThen を有限の長さに制限したもの。
	-}
	instance Enum NutNum where
		succ = Suc

		pred x = case x of 
			Zero -> Zero
			Suc a -> a

		toEnum x = case x of 
			0 -> Zero
			x | x < 0 -> Zero
			  | x > 0 -> Suc $ toEnum (x - 1) 

		fromEnum x = case x of 
			Zero -> 0
			Suc a -> 1 + fromEnum a

		enumFrom x = x : enumFrom (succ x)

		enumFromThen x y 
			| x < y = (!+ x |-| y) !!> x
			| x == y = id !!> x
			| x > y = (!- x |-| y) !!> x

		enumFromTo x y 
			| x > y = x : enumFromTo (pred x) y
			| x == y = x : []
			| x < y = x : enumFromTo (succ x) y

		-- x > y > z || x < y < z が成り立たない場合は空リスト。 y > x > z || y < x < z
		enumFromThenTo x y z 
			| x > y || x > z = []
			| y > z = [x]
			| x <= y || y <= z = x : enumFromThenTo y (y !+ x |-| y) z 

	-- ハイパー演算子
	hyper :: NutNum -> NutNum -> NutNum -> NutNum
	hyper n a b = case n of
		Zero -> error "Boooo!"
		Suc Zero -> case a of
			Zero -> b
			Suc x -> Suc $ hyper n x b
		Suc (Suc Zero) -> case a of
			Zero -> Zero
			Suc x -> tr $ hyper (Suc Zero) (hyper n x b) b
		Suc m -> case a of
			Zero -> Suc Zero
			Suc x -> tr $ hyper m (hyper n x b) b
		where tr = trace (show (fromEnum n) ++ "," ++ show (fromEnum a) ++ "," ++ show (fromEnum b) ++ ".")

	hp :: [NutNum] -> NutNum -> Int -> NutNum
	hp list n m = case m of
		0 -> error $ show list ++ ", Time up." 
		m -> let i = m - 1 in case list of
			a : []        -> a
			a : b : aList -> case a of
				Zero  -> case b of
					Zero           -> error "Boooo!"
					Suc Zero       -> tr $ hp (n : aList) n i
					Suc (Suc Zero) -> tr $ hp (Zero : aList) n i
					Suc y          -> tr $ hp (Suc Zero : aList) n i
				Suc x -> case b of
					Zero     -> error "Boooo!"
					Suc Zero -> tr $ hp ((a !+ n) : aList) n i
					Suc y    -> tr $ hp (x : b : y : aList) n i
			_ -> error "Boooooo!"
			where 
				tr = trace (show list ++ ".")
				-- fromap = map fromEnum

	tarai :: Int -> Int -> Int -> Int
	tarai x y z | x <= y = tr $ y
	            | True   = tr $ tarai (tarai (x - 1) y z) (tarai (y - 1) z x) (tarai (z - 1) x y)
		where 
			tr = trace (show [x,y,z] ++ ".")