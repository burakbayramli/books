# input parameters:
p = dict(formula='x+1', a=0, b=1, n=2, filename='tmp.dat')
from ReadInput import *
input_reader = eval(sys.argv[1])  # PromptUser, ReadInputFile, ...
del sys.argv[1]  # otherwise argparse don't like our extra option
inp = input_reader(p)
a, b, filename, formula, n = inp.get_all()
print inp

from scitools.StringFunction import StringFunction
f = StringFunction(formula)
outfile = open(filename, 'w')
import numpy as np
for x in np.linspace(a, b, n):
    outfile.write('%12g  %12g\n' % (x, f(x)))
outfile.close()

