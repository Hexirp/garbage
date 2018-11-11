module ListFunc where
	type Pred　a = a -> Bool -- Pred = Predicate = 述語。真偽値を返す関数のこと。

	-- iteration = 繰り返し
	iteration :: (a -> a) -> a -> [a]
	iteration f x = x : iteration f (f x)

	infix 8 !!>
	(!!>) :: (a -> a) -> a -> [a]
	f !!> x = iteration f x

	checkExist :: Pred a -> [a] -> Bool
	checkExist f x = case x of
		[] -> False
		(x : xList) -> f x || checkExist f xList

	infix 8 ||>
	(||>) :: Pred a -> [a] -> Bool
	f ||> x = checkExist f x

	checkAll :: Pred a -> [a] -> Bool
	checkAll f x = case x of
		[] -> True
		(x : xList) -> f x && checkAll f xList

	infix 8 &&>
	(&&>) :: Pred a -> [a] -> Bool
	f &&> x = checkAll f x

	folding :: a -> (a -> b -> (b -> s) -> s) -> [a] -> ([b] -> r) -> r
	folding a f [] k = k a
	folding a f (x:xs) k = folding a f xs $ flip (f x) k

	bubble_sort :: (Ord a) => [a] -> ([a] -> r) -> r
	bubble_sort [] k = k []
	bubble_sort x k = let mx = maximum x in bubble_sort (put mx x id) $ k . (mx :)

	put :: (Eq a) => a -> [a] -> ([a] -> r) -> r
	put a [] k = k []
	put a (x:xs) k = if a == x 
		then k xs 
		else put a xs $ k . (x :)

	insert_sort :: (Ord a) => [a] -> ([a] -> r) -> r
	insert_sort = folding [] insert