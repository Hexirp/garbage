module SCPCardGame where
	data Card = Card Name Picture Text
	type Name = String
	type Picture = String

	data Text = None | Text Section Text

	data Board = Hand | Field
	type Hand = [Card]

	data Field = ...