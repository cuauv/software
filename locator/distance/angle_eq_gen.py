from pylab import *
from scipy import *
from scipy import optimize
import argparse
import sys
import os

"""
This file is analogous to eq_gen.py in that it will generate equations for angles
"""

parser = argparse.ArgumentParser()
parser.add_argument('-r','--redTorpedo',action = "store_true")
parser.add_argument('-b','--blueTorpedo',action= "store_true")
parser.add_argument('-w','--wire',action = "store_true")
parser.add_argument('-e','--emperor',action = "store_true")
args = parser.parse_args()




