import DeviceConfig_pb2 as proto

class VariableType:
    def __init__(self, c_type, size, proto_type):
        self.c_type = c_type
        self.size = size
        self.proto_type = proto_type

TYPES = {
    "uint8": VariableType("uint8_t", 1, proto.DeviceConfig.Variable.UINT8),
    "int8": VariableType("int8_t", 1, proto.DeviceConfig.Variable.INT8),
    "uint16": VariableType("uint16_t", 2, proto.DeviceConfig.Variable.UINT16),
    "int16": VariableType("int16_t", 2, proto.DeviceConfig.Variable.INT16),
    "float": VariableType("float", 4, proto.DeviceConfig.Variable.FLOAT),
    }

class ParseError(Exception):
    pass

class Variable:
    def __init__(self, var_block, name, base_register):
        self.name = name

        var_type = var_block["vars"][name]
        if var_type in TYPES:
            self.type = TYPES[var_type]
        else:
            raise ParseError("Invalid variable type " + var_type)

        if "defaults" in var_block and name in var_block["defaults"]:
            self.default = var_block["defaults"][name]
        else:
            self.default = None

        self.base_register = base_register

    def has_default(self):
        return self.default is not None

    def fill_proto_var(self, var):
        var.name = self.name
        var.type = self.type.proto_type
        var.base_register = self.base_register
        if self.has_default():
            if self.type.proto_type is proto.DeviceConfig.Variable.FLOAT:
                var.float_default = self.default
            else:
                var.int_default = self.default

def get_vars(var_block, base_register):
    if not "vars" in var_block:
        raise ParseError("Variable block missing \"vars\" field")
    result = []
    size = 0
    for var_name in var_block["vars"]:
        var = Variable(var_block, var_name, base_register)
        result.append(var)
        base_register += var.type.size
        size += var.type.size
    return (result, size)

class ReadGroup:
    def __init__(self, groups, name, base_register):
        self.name = name
        if not "interval_ms" in groups[name]:
            raise ParseError("Interval missing in read group " + name)
        self.interval = groups[name]["interval_ms"]
        (self.read_vars, self.size) = get_vars(groups[name], base_register)

    def fill_proto_group(self, group):
        group.interval_ms = self.interval
        for var in self.read_vars:
            var.fill_proto_var(group.read_variables.add())

class Config:
    def __init__(self, cfg):
        if not "name" in cfg:
            raise ParseError("Missing device name")
        self.name = cfg["name"]

        if "writeVars" in cfg:
            (self.write_vars, self.num_write_regs) = get_vars(cfg["writeVars"], 0)
        else:
            self.write_vars = []
            self.num_write_regs = 0

        self.read_groups = []
        read_base_reg = 0
        if "readGroups" in cfg:
            for group_name in cfg["readGroups"]:
                group = ReadGroup(cfg["readGroups"], group_name, read_base_reg)
                read_base_reg += group.size
                self.read_groups.append(group)

        self.num_read_regs = read_base_reg

        # 3 is heartbeat, any read can return up to num_read registers
        self.tx_buffer_size = max(3, self.num_read_regs) + 4
        # 2 is range read, 2*num_write is length of indexed write, 1+num_write is length of range write, num_read is indexed read
        self.rx_buffer_size = max(2, 2*self.num_write_regs, self.num_write_regs+1, self.num_read_regs) + 4

        p = self.get_proto()
        proto_as_ints = [ord(x) for x in p.SerializeToString()]
        self.proto_array = ','.join([str(x) for x in proto_as_ints])
        self.proto_checksum = sum(proto_as_ints) % 256

    def get_proto(self):
        cfg = proto.DeviceConfig()
        cfg.name = self.name
        cfg.type = "CUAUV-AVR-v1"
        for var in self.write_vars:
            var.fill_proto_var(cfg.write_variables.add())
        for group in self.read_groups:
            group.fill_proto_group(cfg.read_groups.add())
        return cfg
