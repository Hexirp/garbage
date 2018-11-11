module WebData where
	-- HaskellHyperText = HHT
	data WebPage = PageInfo | HyperText
	data PageInfo = Info Meta Title AutherInfo
	data Meta = Meta CherSet
	data Title = Title Sentence
	data AutherInfo = AuInfo Name EMail