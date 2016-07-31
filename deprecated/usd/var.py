class Var:
    def __init__(self, group, name):
        self.group = group
        self.name = name
        self.vartype = None
        self.ctype = None
        self.size = None
        self.readonly = False
        self.address = None
        self.interval = 0
        self.filter = ""
        self.iwrite = True
