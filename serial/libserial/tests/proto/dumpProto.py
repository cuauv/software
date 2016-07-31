#!/usr/bin/env python

import argparse
import toml
import DeviceConfig_pb2 as proto

VARTYPES = {"uint8":  proto.DeviceConfig.Variable.UINT8,
            "int8":   proto.DeviceConfig.Variable.INT8,
            "uint16": proto.DeviceConfig.Variable.UINT16,
            "int16":  proto.DeviceConfig.Variable.INT16,
            "float":  proto.DeviceConfig.Variable.FLOAT,
           }

VARSIZES = {"uint8":  1,
            "int8":   1,
            "uint16": 2,
            "int16":  2,
            "float":  4,
           }

def makeParser():
    parser = argparse.ArgumentParser(description="Convert a device toml cfg into a binary device proto file")
    parser.add_argument('device_config', help="The input device config file")
    parser.add_argument('proto_file', help="The file to write the binary config to")
    return parser

if __name__ == '__main__':
    parser = makeParser()
    args = parser.parse_args()
    with open(args.device_config, 'r') as infile:
        with open(args.proto_file, 'wb') as outfile:
            incfg = toml.loads(infile.read())
            cfg = proto.DeviceConfig()
            cfg.name = incfg["name"]
            cfg.type = "fake_proto"
            current_reg = 0
            for varname in incfg["writeVars"]["vars"]:
                invar = incfg["writeVars"]["vars"][varname]
                var = cfg.write_variables.add()
                var.name = varname
                var.type = VARTYPES[invar]
                var.base_register = current_reg
                current_reg = current_reg + VARSIZES[invar]

            current_reg = 0
            for groupname in incfg["readGroups"]:
                ingroup = incfg["readGroups"][groupname]
                group = cfg.read_groups.add()
                group.interval_ms = ingroup["interval_ms"]
                for varname in ingroup["vars"]:
                    invar = ingroup["vars"][varname]
                    var = group.read_variables.add()
                    var.name = varname
                    var.type = VARTYPES[invar]
                    var.base_register = current_reg
                    current_reg = current_reg + VARSIZES[invar]

            outfile.write(cfg.SerializeToString())
