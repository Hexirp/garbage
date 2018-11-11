module Pool where
	import Fuzzy

	-- 確率的ブーリアン及びファジイ論理

	pBool x = case x of 
		False -> 0
		True -> 1

	pand x y = x * y

	pnot x = 1 - x

	por x y = pnot $ pand (pnot x) (pnot y)

	pxor x y = pand (por x y) (pnot $ pand x y)

	pex x = pBool $ 0 < x -- pool + exist

	pal x = pBool $ 1 <= x -- pool + all

	pfl x = pand x $ pnot x

	ptr x = por x $ pnot x

	instance Fuzzy Double where
		fand x y = x * y
		fnot x = 1 - x