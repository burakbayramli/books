"""Throwaway script to test BIC score search
"""

#from gPy.Data import Data
from gPy.Data import CompactFactor
import sys, gzip
from gPy.IO import read_csv

data = CompactFactor(read_csv(gzip.open('/home/jc/godot/research/icml08/data/insurance_100.data.gz')))
for v in data.variables():
    print v
    print
    print data.bic_search(v)
