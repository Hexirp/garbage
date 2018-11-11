-- Fuzzy = Fuzzy logic = ファジイ論理、真偽が連続的な値で表される論理。
class FuzzyLogic a where
	fnot :: a -> a

	fand :: a -> a -> a
	fand x y = fnot $ for (fnot x) (fnot y)

	for :: a -> a -> a
	for x y = fnot $ fand (fnot x) (fnot y)

	fimp :: a -> a -> a
	fimp x y = for (fnot x) y

	{- 
		x ~&~ y = y ~&~ x
		x ~&~ (y ~&~ z) = (x ~&~ y) ~&~ z
		x >= y = (x ~&~ z) >= (y ~&~ z)
		x ~&~ 1 = x
		(fnot . fnot) x = x
	-}