#!/usr/bin/env python
import sys
import argparse
from os.path import dirname, realpath, join, isdir
D = realpath(join(dirname(realpath(__file__)), '../'))
assert isdir(join(D, 'femlib'))
sys.path.insert(0, D)
from viewer import launch_viewer
parser = argparse.ArgumentParser(sys.argv[1:])
parser.add_argument("sources", nargs="*")
args = parser.parse_args(sys.argv[1:])
launch_viewer(args.sources)
