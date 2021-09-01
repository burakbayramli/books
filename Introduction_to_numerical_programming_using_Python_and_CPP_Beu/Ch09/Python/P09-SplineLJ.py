# Cubic spline interpolation of Lennard-Jones potential
from modfunc import *

def func(x): return 1e0/pow(x,12)-1e4/pow(x,6)      # Lennard-Jones potential

# main

n  = 7                                                # number of data points
ni = 100                                          # number of sampling points

a = [0]*(n+1); b = [0]*(n+1)                            # spline coefficients
c = [0]*(n+1); d = [0]*(n+1)
x = [0]*(n+1); y = [0]*(n+1)                     # coordinates of data points
xi = [0]*(ni+1); yi = [0]*(ni+1)                       # interpolation points

x[1] = 0.21; x[2] = 0.24; x[3] = 0.27; x[4] = 0.30
x[5] = 0.33; x[6] = 0.36; x[7] = 0.39
for i in range(1,n+1): y[i] = func(x[i])

h = (x[n]-x[1])/(ni-1)
for i in range(1,ni+1): xi[i] = x[1] + (i-1)*h      # interpolation arguments

Spline(x,y,n,0e0,0e0,0,a,b,c,d,xi,yi,ni)                    # natural splines

out = open("interpol.txt","w")                             # open output file
out.write("      x           y           xi          yi          f\n")
for i in range(1,ni+1):
   if (i <= n):
      out.write("{0:12.3e}{1:12.3e}{2:12.3e}{3:12.3e}{4:12.3e}\n".
                format(x[i],y[i],xi[i],yi[i],func(xi[i])))
   else:
      out.write("{0:36.3e}{1:12.3e}{2:12.3e}\n".
                format(xi[i],yi[i],func(xi[i])))
out.close()
