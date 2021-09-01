# Intensity distribution for Fraunhofer diffraction
from math import *
from roots import *
from graphlib import *

def Intens(x):                                                    # intensity
   global I0                                              # maximum intensity
   sinc = sin(x)/x if x else 1e0
   return I0 * sinc * sinc
def dIntens(x):                                        # derivative of Intens
   global I0                                              # maximum intensity
   if (x):
      sinc = sin(x)/x
      return 2e0 * I0 * (cos(x) - sinc) * sinc / x
   else: return 0e0
def func(x):                            # function for half-width calculation
   global I0                                              # maximum intensity
   return Intens(x) - 0.5e0 * I0

# main

GraphInit(800,600) 

I0 = 1e0                                                  # maximum intensity
xmin = -10e0; xmax = 10e0 

#----------------------------------------------------------------------------
n = 101
x = [0]*(2*n+1); y = [0]*(2*n+1)

h = (xmax-xmin) / (n-1)
for i in range(1,n+1):
  xi = xmin + (i-1)*h
  x[i  ] = xi; y[i  ] = Intens(xi)
  x[i+n] = xi; y[i+n] = dIntens(xi)

nn = [0, n, 2*n]; col = ["", "red", "blue"]; sty = [0, 1,-1]
MultiPlot(x,y,y,nn,col,sty,2,10,
          0e0,0e0,0,0e0,0e0,0,0.15,0.85,0.15,0.85,
          "x","I, I'","Fraunhofer diffraction")

#----------------------------------------------------------------------------
xi = pi/2e0
(xi,ierr) = Secant(func,0e0,pi,xi)                 # half-width of major peak
print("Half-width of major peak = {0:8.5f}".format(xi))

#----------------------------------------------------------------------------
print("\nPositions of intensity maxima:")
h = 0.5e0
a = xmin
while (a < xmax):                                      # root separation loop
   b = a + h                                      # new search interval [a,b]
   (xi,ierr) = FalsPos(dIntens,a,b)
   if ((ierr == 0) and (xi != b) and (Intens(xi) > 1e-10)):
     print("{0:8.5f}  in  ({1:5.2f},{2:5.2f})".format(xi,a,b))
   a = b                                                # shift left boundary

MainLoop()
