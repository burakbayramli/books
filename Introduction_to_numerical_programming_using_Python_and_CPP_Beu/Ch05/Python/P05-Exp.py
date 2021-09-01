# Evaluates the exponential function
from math import *
from elemfunc import *

# main

for i in range(-20,25,5):
  x = float(i)
  expx = exp(x)                                                    # built-in
  Expx = Exp(x)                                                # power series
  Exp1x = Exp1(x)                                        # continued fraction
  print(x,Exp1x,1e0-Exp1x/expx)
