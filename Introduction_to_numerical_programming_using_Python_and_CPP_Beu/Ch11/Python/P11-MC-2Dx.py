# Two-dimensional Monte Carlo quadrature with variance reduction
from math import *
from random import *
from random1 import *
from modfunc import *
from graphlib import *

pi4 = 4 * pi

def func(x,y):                                                    # integrand
   r2 = x*x + y*y
   return r2*exp(-0.5e0*r2)/pi4

# main

L = 8e0                                  # integration domain [-L,L] x [-L,L]
L2 = L * L                                          # area of sampling domain

nn  = [0]*4                                         # ending indexes of plots
col = [""]*4                                                # colors of plots
sty = [0]*4                                                 # styles of plots

np = 400                                          # number of plotting points
x1 = [0]*(3*np+1); y1 = [0]*(3*np+1)                        # plotting points
x2 = [0]*(3*np+1); y2 = [0]*(3*np+1); sig = [0]*(3*np+1)

seed()

out = open("mcarlo.txt","w")                               # open output file
out.write("     n       Int       sig      Int_w     sig_w\n")

for ip in range(1,np+1):
   n = 250 * ip                                   # number of sampling points

   f1 = f2 = 0.0          # quadrature with uniform sampling in [0,L] x [0,L]
   for i in range(1,n+1):
      x = L * random(); y = L * random()
      f = func(x,y)                                               # integrand
      f1 += f; f2 += f * f                                             # sums

   f1 /= n; f2 /= n                                                # averages
   s = 4e0 * L2 * f1                                               # integral
   sigma = 4e0 * L2 * sqrt(fabs(f2-f1*f1)/n)             # standard deviation
   out.write(("{0:8d}{1:10.5f}{2:10.5f}").format(n,s,sigma))
   x1[ip] =       n ; y1[ip] = s
   x2[ip] = log10(n); y2[ip] = log10(sigma)

   f1 = f2 = 0.0                          # quadrature with Gaussian sampling
   for i in range(1,n+1):
      (w, x, y) = randNrm2()        # random numbers with normal distribution
      f = func(x,y) / w                                           # integrand
      f1 += f; f2 += f * f                                             # sums

   f1 /= n; f2 /= n                                                # averages
   s = f1                                                          # integral
   sigma = sqrt((f2-f1*f1)/n)                            # standard deviation
   out.write(("{0:10.5f}{1:10.5f}").format(s,sigma))
   x1[np+ip] =       n ; y1[np+ip] = s
   x2[np+ip] = log10(n); y2[np+ip] = log10(sigma)

   f1 = f2 = 0.0                       # quadrature with exponential sampling
   for i in range(1,n+1):
      x = randExp()          # random variables with exponential distribution
      y = randExp()
      w = exp(-(x+y))
      f = func(x,y) / w                                           # integrand
      f1 += f; f2 += f * f                                             # sums

   f1 /= n; f2 /= n                                                # averages
   s = 4e0 * f1                                                    # integral
   sigma = 4e0 * sqrt((f2-f1*f1)/n)                      # standard deviation
   out.write(("{0:10.5f}{1:10.5f}\n").format(s,sigma))
   x1[2*np+ip] =       n ; y1[2*np+ip] = s
   x2[2*np+ip] = log10(n); y2[2*np+ip] = log10(sigma)

out.close()
                                                          # linear regression
(a, b, sigma, sigmb, chi2) = LinFit(x2[0:],y2[0:],sig,np,0)
print("sigma = {0:6.3f} n**({1:6.3f})  uniform sampling". \
      format(pow(10e0,b),a))

(a, b, sigma, sigmb, chi2) = LinFit(x2[np:],y2[np:],sig,np,0)
print("sigma = {0:6.3f} n**({1:6.3f})  Gaussian sampling". \
      format(pow(10e0,b),a))

(a, b, sigma, sigmb, chi2) = LinFit(x2[2*np:],y2[2*np:],sig,np,0)
print("sigma = {0:6.3f} n**({1:6.3f})  exponential sampling". \
      format(pow(10e0,b),a))

GraphInit(1200,600)

nn[1] =   np; col[1] = "blue" ; sty[1] = 0                 # uniform sampling
nn[2] = 2*np; col[2] = "red"  ; sty[2] = 0                # Gaussian sampling
nn[3] = 3*np; col[3] = "green"; sty[3] = 0             # exponential sampling
MultiPlot(x1,y1,y1,nn,col,sty,3,10,0e0,0e0,0,0e0,0e0,0,
          0.10,0.45,0.15,0.85,"n","Int","Monte Carlo - integral")

MultiPlot(x2,y2,y2,nn,col,sty,3,10,0e0,0e0,0,0e0,0e0,0,
          0.60,0.95,0.15,0.85,"log(n)","log(sig)",
          "Monte Carlo - standard deviation")

MainLoop()
