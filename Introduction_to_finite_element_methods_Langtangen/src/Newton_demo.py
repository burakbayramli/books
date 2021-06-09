import sys
import matplotlib.pyplot as plt
from numpy import *

from sys import argv
if not len(argv) == 6:
  print("usage: > Newton_demo.py f dfx x0 xmin xmax ")
  sys.exit(0)

f_str = argv[1]
dfdx_str = argv[2]
x0 = float(argv[3])
xmin = float(argv[4])
xmax = float(argv[5])

i = 0
tol = 1.0e-9
maxit = 100
x = x0
f = eval(f_str, vars())
dfdx = eval(dfdx_str, vars())
xs = []
fs = []
xs.append(x)
fs.append(f)
print("x=%.3e   f=%.3e   dfdx=%.3e " %  (x, f, dfdx))
while abs(f) > tol and i <= maxit and x > xmin and x < xmax :
  x = x0 - f/dfdx
  f = eval(f_str, vars())
  dfdx = eval(dfdx_str, vars())
  x0 = x
  xs.append(x0)
  fs.append(f)
  i = i+1
  print("x=%.3e   f=%.3e   dfdx=%.3e " %  (x, f, dfdx))

x = arange(xmin, xmax, (xmax-xmin)/100.0)
f = eval(f_str, vars())

plt.plot(x, f, "g")
plt.plot(xs, fs, "bo")
plt.plot(xs, fs, "b")
plt.show()
