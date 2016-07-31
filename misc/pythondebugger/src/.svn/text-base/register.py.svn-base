class register:
	def __init__(self, addr, halfwords, datatype, name, isPointer, isReadOnly, comment=""):
		self.halfwords = halfwords
		self.type = datatype
		self.name = name
		self.isPointer = isPointer
		self.isReadOnly = isReadOnly
		self.comment = comment
		self._addr = addr
	def __str__(self):
		return self.name
	def __repr__(self):
		return self.name