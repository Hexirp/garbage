module Speed where
	infixl 6 |+|
	a |+| b = (a + b) / (1.0 + a * b / photon ^ 2)

	photon = 100.0