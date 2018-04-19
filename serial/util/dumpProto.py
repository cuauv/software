#!/usr/bin/env python2

import argparse
import toml

import config

def makeParser():
    parser = argparse.ArgumentParser(description="Convert a device toml cfg into a binary device proto file")
    parser.add_argument('device_config', help="The input device config file")
    parser.add_argument('proto_file', help="The file to write the binary config to")
    return parser

def main():
    parser = makeParser()
    args = parser.parse_args()
    with open(args.device_config, 'r') as infile:
        with open(args.proto_file, 'wb') as outfile:
            incfg = toml.loads(infile.read())
            cfg = config.Config(incfg)
            outfile.write(cfg.get_proto().SerializeToString())

if __name__ == '__main__':
    main()
