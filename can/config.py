import json

def parse_shmvar(var):
    return var.split('.')

def load_config(hardware, devices):
    hardware_json = json.load(open(hardware, 'r'))
    devices_json = json.load(open(devices, 'r'))

    idmap = hardware_json['idmap']
    types = hardware_json['types']

    cfg = {'devs': [], 'groups': []}

    # Parse out list of devices
    for device in devices_json['devices']:
        dev = devices_json['devices'][device]
        dtype = types[dev['type']]
        def has_binding(name):
            return dtype['bindings'][name] in dev['vars']
        def read_binding(name):
            return parse_shmvar(dev['vars'][dtype['bindings'][name]])
        devcfg = {}
        devcfg['name'] = dev['dev']
        devcfg['id'] = idmap[dev['dev']]
        devcfg['type'] = dev['type']
        devcfg['other_vars'] = dev['other_vars']
        for binding in dtype['bindings']:
            if has_binding(binding):
                devcfg[binding] = read_binding(binding)
                devcfg[binding + '_name'] = dtype['bindings'][binding]
        devcfg['status'] = parse_shmvar(dev['status'])
        devcfg['hwid'] = dtype['hwid']
        cfg['devs'].append(devcfg)

    # Assign CAN groups based on setpoint shm groups
    nextgroup = 2
    shmgroups = {}
    for device in cfg['devs']:
        shmgroup = device['setpoint'][0]
        if not shmgroup in shmgroups:
            group = {'id': nextgroup, 'shmgroup': shmgroup, 'varnames': []}
            nextgroup = nextgroup+1
            cfg['groups'].append(group)
            shmgroups[shmgroup] = group
        else:
            group = shmgroups[shmgroup]
        # Assign group/offset to device
        device['group'] = group['id']
        device['offset'] = len(group['varnames'])
        group['varnames'].append((len(group['varnames']), device['setpoint'][1]))
        # Limit groups to only have 4 devices
        if len(group['varnames']) == 4:
            del shmgroups[shmgroup]

    return cfg
