module Game where
	class Transition st op where
		next :: st -> op -> st

	class Simulation st where
		-- adTime = advanceTheTime
		adTime :: st -> st
		adTime s = next s ()

	class Game st op where
		-- Op = Operation
		playerOp :: st -> IO op -> st

	-- Pl = Player , Num = Number
	newtype PlNum = Int
	newtype PlName = String
	newtype PlList = [(PlNumber,PlName)]

	newtype Phasing = PrayerNum

	-- Piece = 駒 , Phasing = 手番 , Othel = Othello = オセロ
	data OthelPiece = None | White | Black
	newtype OthelField = [[OthelPiece]]
	newtype OthelOp = (OthelPiece,Int,Int)
	newtype OthelState = (Phasing,OthelField)

	othelOp :: OthelState -> OthelOp -> OthelState
	othelOp s o = if otelCheckOp s o
		then othelDoOp s o
		else error "You can't your opretation!"