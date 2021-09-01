# One-dimensional Monte Carlo quadrature with variance reduction
from math import *
from random import *
from modfunc import *
from graphlib import *

#============================================================================
def ranSqrt():
#----------------------------------------------------------------------------
#  Returns a real random number x in the range [0,1) with the distribution 
#  w(x) = 3/2 x^(1/2), and the corresponding value w(x)
#----------------------------------------------------------------------------
   x = pow(random(),2e0/3e0)
   w = 1.5e0 * sqrt(x)
   return (x, w)

def func(x): return x * exp(-x)                                   # integrand

# main

nn  = [0]*3                                         # ending indexes of plots
col = [""]*3                                                # colors of plots
sty = [0]*3                                                 # styles of plots

np = 400                                          # number of plotting points
x1 = [0]*(2*np+1); y1 = [0]*(2*np+1)                        # plotting points
x2 = [0]*(2*np+1); y2 = [0]*(2*np+1); sig = [0]*(2*np+1)

seed()

out = open("mcarlo.txt","w")                               # open output file
out.write("     n       Int       sig      Int_w     sig_w\n")

for ip in range(1,np+1):
   n = 250 * ip                                   # number of sampling points

   f1 = f2 = 0e0                           # quadrature with uniform sampling
   for i in range(1,n+1):
      x = random()                   # RNG with uniform distribution in [0,1)
      f = func(x)                                                 # integrand
      f1 += f; f2 += f * f                                             # sums

   f1 /= n; f2 /= n                                                # averages
   s = f1                                                          # integral
   sigma = sqrt((f2-f1*f1)/n)                            # standard deviation
   out.write(("{0:8d}{1:10.5f}{2:10.5f}").format(n,s,sigma))
   x1[ip] =       n ; y1[ip] = s
   x2[ip] = log10(n); y2[ip] = log10(sigma)

   f1 = f2 = 0e0                        # quadrature with importance sampling
   for i in range(1,n+1):
      (x, w) = ranSqrt()                         # RNG with distribution w(x)
      if (w):
         f = func(x) / w                                          # integrand
         f1 += f; f2 += f * f                                          # sums

   f1 /= n; f2 /= n                                                # averages
   s = f1                                                          # integral
   sigma = sqrt((f2-f1*f1)/n)                            # standard deviation
   out.write(("{0:10.5f}{1:10.5f}\n").format(s,sigma))
   x1[np+ip] =       n ; y1[np+ip] = s
   x2[np+ip] = log10(n); y2[np+ip] = log10(sigma)

out.close()
                                                          # linear regression
(a, b, sigma, sigmb, chi2) = LinFit(x2[0:],y2[0:],sig,np,0)
print("sigma = {0:6.3f} n**({1:6.3f})  w(x) = 1".format(pow(10e0,b),a))

(a, b, sigma, sigmb, chi2) = LinFit(x2[np:],y2[np:],sig,np,0)
print("sigma = {0:6.3f} n**({1:6.3f})  w(x) = (3/2) x**(1/2)". \
      format(pow(10e0,b),a))

GraphInit(1200,600)

nn[1] =   np; col[1] = "blue"; sty[1] = 0                  # uniform sampling
nn[2] = 2*np; col[2] = "red" ; sty[2] = 0               # importance sampling
MultiPlot(x1,y1,y1,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
          0.10,0.45,0.15,0.85,"n","Int","Monte Carlo - integral")

MultiPlot(x2,y2,y2,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
          0.60,0.95,0.15,0.85,"log(n)","log(sig)",
          "Monte Carlo - standard deviation")

MainLoop()
