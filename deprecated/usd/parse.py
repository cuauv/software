import re
import string
from device import Device
from var import Var

sizes=dict(float=2, bool=1, int16=1, int32=2, uint16=1, uint32=2)
ctypes=dict(float='double', bool='bool', int16='int', int32='int')
bools=dict(Y=True, y=True, N=False, n=False)

def parse(f):
    def add_var(d, v):
        # Do nothing if var is none. Gracefully handles first variable,
        # and after that point var is never None
        if v is None:
            return

        if v.ctype == 'string' and v.size is None:
            raise ValueError('String %s has no length' % v.name)
        if v.address is None:
            raise ValueError('Var %s has no address' % v.name)
        if v.vartype is None:
            raise ValueError('Var %s has no type' % v.name)

        d.vars.append(v)

    device = None
    var = None
    group = None
    custom = False
    cur_address = 0
    cur_interval = 0
    cur_filter = None
    cur_iwrite = None

    for line in f:
        line = line.strip()
        if line == '':
            pass
        elif line[0] == '$':
            pass
        elif line[0] == '@':
            line_result = line.split(',',3)
            for i in range(len(line_result)):
                line_result[i] = line_result[i].strip('@'+string.whitespace)
            if device is None:
                device = Device(line_result[0])
                group = line_result[0]
            else:
                err = 'Duplicate device line in file (Had %s, now have %s)' \
                    % (device.name, line_result[0])
                raise ValueError(err)
            device.id = int(line_result[1])
            if line_result[2] == '/dev/ttyUSB0':
                device.path = 'autodetect'
            else:
                device.path = line_result[2]
            device.baud_rate = int(line_result[3])
        elif line[0] == ';':
            line = line.strip(';'+string.whitespace)
            if line.startswith('group:'):
                line_result = line[6:].split(',',2)
                for i in range(len(line_result)):
                    line_result[i] = line_result[i].strip()
                group = line_result[0]
                if device is None:
                    raise ValueError('Group %s has no device' % group)
                else:
                    if bools[line_result[1]] == True:
                        if group not in device.write_only_groups:
                            device.write_only_groups.append(group)
                    else:
                        if group in device.write_only_groups:
                           raise ValueError('Group %s is already write only' % group)
            elif line.startswith('filter:'):
                line_result = line[7:].strip()
                cur_filter = line_result
            elif line.startswith('no_initial_write'):
                cur_iwrite = True
        elif line[0] == '#':
            line_result = line.split(',',3)
            for i in range(len(line_result)):
                line_result[i] = line_result[i].strip('#'+string.whitespace)
            cur_address = int(line_result[0])
            cur_interval = int(line_result[1])
            custom = bools[line_result[2]]
        elif custom == True:
            line_result = line.split(',',5)
            for i in range(len(line_result)):
                line_result[i] = line_result[i].strip()
            (g,v) = line_result[2].split('/',1)
            if device is None:
                raise ValueError('Var %s has no device' % g)
            var = Var(g, v)
            var.vartype = line_result[1]
            if line_result[1] == 'string':
                var.ctype = 'string'
            else:
                var.ctype = ctypes[line_result[1]]
            var.size = int(line_result[0])
            var.readonly = bools[line_result[4]]
            var.address = cur_address
            cur_address += int(line_result[0])
            var.interval = cur_interval
            if cur_filter is not None:
                var.filter = cur_filter
                cur_filter = None
            if cur_iwrite is not None:
                var.iwrite = False
                cur_iwrite = None
            add_var(device, var)

    if device.path is None:
        raise ValueError('Device %s has no path' % device.name)
    if device.id is None and device.path == 'autodetect':
        raise ValueError('Device %s is autodetect but no id' % device.name)

    return device
