#!/usr/bin/env python2
import os
import sys
import imp
import argparse
from csvutil import *

""" Parses arguments """
def parse():
    parser = argparse.ArgumentParser(description='')
    parser.add_argument(dest='input', type=str, nargs='+', help='module to learn')
    return vars(parser.parse_args())

""" Ensures directory exists for file writing """
def ensure_dir(f):
    d = os.path.dirname(f)
    if not os.path.exists(d):
        os.makedirs(d)

""" Attempts to load in config files """
def loadfile(f):
    try:
        f = open(f)
        data = []
        for line in f:
            data.append(line.strip())
        return data
    except:
        print "Error loading file: %s" % f
        sys.exit(0)

""" Loads all configs for a single module """
def loadcfg(path):
    models = loadfile(os.path.join(path, 'conf', 'models.conf'))
    datafiles = loadfile(os.path.join(path, 'conf', 'data.conf'))
    return (models, datafiles)

""" Imports a file """
def importfile(name):
    fp, pathname, desc = imp.find_module(name)
    try:
        return imp.load_module(name, fp, pathname, desc)
    finally:
        if fp:
            fp.close()

""" Writes the model """
def write_model(m, d, f):
    f = os.path.join(d, f)
    ensure_dir(f)
    m.save(f)

""" Learns and writes the model """
def learn(module, m, Xlist, Ylist, d, f):
    MODEL = importfile(os.path.join(module, 'pymodel', m))
    model = MODEL.Model(Xlist, Ylist)
    write_model(model, d, f)

""" Cleans the data and model directory of the module """
def clean(modules):
    for module in modules:
        datapath = os.path.join(module, 'data')
        modelpath = os.path.join(module, 'models')
        ensure_dir(datapath)
        ensure_dir(modelpath)
        os.system('rm %s' % os.path.join(datapath, '*'))
        os.system('rm -r %s' % os.path.join(modelpath, '*'))

""" Main """
def main():
    args = parse()
    clean(args['input'])

    for module in args['input']:
        print "Learnng module: %s" % module
        (models, datafiles) = loadcfg(module)

        FEATURES = importfile(os.path.join(module, 'features'))

        if datafiles == []:
          sys.exit(0)

        data = []

        for datafile in datafiles:
            i = os.path.join('data', '%s.shmlog' % datafile)
            o = os.path.join(module, 'data', '%s.csv' % datafile)
            var = os.path.join(module, 'conf', 'vars.conf')

            os.system("auv-shmlog-tocsv -i %s -o %s %s" % (i, o, var))

            print "Processing data file: %s" % o
            labels, csvdata = load_csv(o)
            data.append(csvdata)

        features = FEATURES.Features(labels, data)

        for m in models:
            for f in features.Ylist:
                print "Generating a %s model for feature %s" % (m, f)
                outdir = os.path.join(module, 'models', f, m)
                learn(module, m, features.Xlist, features.Ylist[f], outdir, 'model')

        labellist = [0] * len(labels)
        for k in labels:
            labellist[labels[k]] = k
        f = open(os.path.join(module, 'models', 'labels.conf'), 'w')
        f.write('\n'.join(labellist))
        f.close()

if __name__ == "__main__":
   main()
