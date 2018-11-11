module Lambda where
	lmdZero :: (a -> a) -> a -> a
	lmdZero f x = x

	lmdSucc :: ((a -> a) -> a -> a) -> (a -> a) -> a -> a
	lmdSucc n f x = f $ n f x

	lmdPlus :: ((a -> a) -> a -> a) -> ((a -> a) -> a -> a) -> (a -> a) -> a -> a
	lmdPlus m n = m lmdSucc n