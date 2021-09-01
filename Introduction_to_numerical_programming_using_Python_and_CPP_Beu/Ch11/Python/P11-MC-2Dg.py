# Monte Carlo calculation of the unit circle area
from math import *
from random import *
from graphlib import *
from modfunc import *

# main

np = 400                                          # number of plotting points
x1 = [0]*(np+1); y1 = [0]*(np+1)                            # plotting points
x2 = [0]*(np+1); y2 = [0]*(np+1); sig = [0]*(np+1)

seed()

out = open("mcarlo.txt","w")                               # open output file
out.write("     n       Int       sig      Int_w     sig_w\n")

for ip in range(1,np+1):
   n = 250 * ip                                   # number of sampling points

   ni = 0                                         # number of interior points
   for i in range(1,n+1):
      x = random(); y = random()
      if (x*x + y*y <= 1e0): ni += 1                     # add interior point

   fi = ni/n                                    # fraction of interior points
   s = 4e0 * fi                                                    # integral
   sigma = 4e0 * sqrt((fi - fi*fi)/n)                    # standard deviation
   out.write(("{0:8d}{1:10.5f}{2:10.5f}").format(n,s,sigma))
   x1[ip] =       n ; y1[ip] = s
   x2[ip] = log10(n); y2[ip] = log10(sigma)

out.close()
                                                          # linear regression
(a, b, sigma, sigmb, chi2) = LinFit(x2,y2,sig,np,0)
print("sigma = {0:6.3f} n**({1:6.3f})  w(x) = 1\n".format(pow(10e0,b),a))

GraphInit(1200,600)

Plot(x1,y1,np,"blue",0,0.10,0.45,0.15,0.85,"n","Int",
     "Monte Carlo - unit circle area")

Plot(x2,y2,np,"blue",0,0.60,0.95,0.15,0.85,"log(n)","log(sig)",
     "Monte Carlo - standard deviation")

MainLoop()
