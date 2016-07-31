class Device:
    def __init__(self, name):
        self.name = name
        self.id = None
        self.path = None
        self.baud_rate = 57600
        self.vars = []
        self.write_only_groups = []

    def get_groups(self):
        return set(v.group for v in self.vars)

    def is_writeonly_group(self, group):
        return group in self.write_only_groups
