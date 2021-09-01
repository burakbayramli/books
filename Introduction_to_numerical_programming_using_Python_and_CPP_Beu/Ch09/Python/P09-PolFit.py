# Polynomial fit
from modfunc import *
from graphlib import *

# main

nn  = [0]*3                                      # end-indexes of the 3 plots
col = [""]*3                                                # colors of plots
sty = [0]*3                                                 # styles of plots

n = 6                                               # number of observed data
npar = 4                                         # number of model parameters
nfit = 100                              # number of points plotted from model

x     = [0]*(n+nfit+1)                       # x-coordinates of observed data
y     = [0]*(n+nfit+1)                       # y-coordinates of observed data
sigmy = [0]*(n+1)                      # standard deviations of observed data
a     = [0]*(npar+1)                                       # model parameters
sigma = [0]*(npar+1)                            # uncertainties of parameters

                  # observed data generated from P(x) = 0.1 x^3 - x^2 + 0.5 x
x[1] = 0e0; y[1] =  0.0e0; x[2] =  2e0; y[2] =  -2.2e0
x[3] = 4e0; y[3] = -7.6e0; x[4] =  6e0; y[4] = -11.4e0
x[5] = 8e0; y[5] = -8.8e0; x[6] = 10e0; y[6] =   5.0e0

iopt = 0                              # least squares fit: equal errors sigmy
chi2 = PolFit(x,y,sigmy,n,iopt,a,sigma,npar)

print("Polynomial fit:")
for i in range(1,npar+1):
   print(("a[{0:d}] = {1:8.4f} +/- {2:8.4f}").format(i,a[i],sqrt(sigma[i])))
print("Chi^2 = {0:8.4f}".format(chi2))

h = (x[n]-x[1])/(nfit-1)
for i in range(1,nfit+1):                               # append model points
   xi = x[1] + (i-1)*h
   f = a[1]
   for j in range(2,npar+1): f = f*xi + a[j]                 # evaluate model
   x[n+i] = xi; y[n+i] = f

GraphInit(800,600)

nn[1] = n       ; col[1] = "red" ; sty[1] = 4                 # observed data
nn[2] = n + nfit; col[2] = "blue"; sty[2] = 1                  # fitted model

MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
          0.15,0.95,0.15,0.85,"x","y","Polynomial fit")

MainLoop()
