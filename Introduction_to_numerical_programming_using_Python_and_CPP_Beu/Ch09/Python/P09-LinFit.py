# Linear fit of a model to observed data points
from modfunc import *
from graphlib import *

# main

nn  = [0]*4                                      # end-indexes of the 3 plots
col = [""]*4                                                # colors of plots
sty = [0]*4                                                 # styles of plots

n = 5                                               # number of observed data
nfit = 2                                # number of points plotted from model
n1 = n + nfit; n2 = n + 2*nfit                                  # end indexes

x = [0]*(n2+1); y = [0]*(n2+1)                                # observed data
sigmy = [0]*(n+1)                      # standard deviations of observed data

x[1] = 1e0; y[1] = 0.8e0                                        # data points
x[2] = 2e0; y[2] = 2.1e0
x[3] = 3e0; y[3] = 2.8e0
x[4] = 4e0; y[4] = 4.0e0
x[5] = 5e0; y[5] = 4.4e0

iopt = 0                              # least squares fit: equal errors sigmy
(a, b, sigma, sigmb, chi2) = LinFit(x,y,sigmy,n,iopt)

print("Least squares fit:")
print("a = {0:8.4f} +/- {1:8.4f}".format(a,sigma))
print("b = {0:8.4f} +/- {1:8.4f}".format(b,sigmb))
print("Chi^2 = {0:8.4f}".format(chi2))

h = (x[n]-x[1])/(nfit-1)
for i in range(1,nfit+1):                               # append model points
   x[n+i] = x[1] + (i-1)*h
   y[n+i] = a*x[n+i] + b                                    # regression line

for i in range(1,n+1): sigmy[i] = 0.15*y[i]    # generate standard deviations

iopt = 1                             # Chi-square fit: different errors sigmy
(a, b, sigma, sigmb, chi2) = LinFit(x,y,sigmy,n,iopt)

print("\nChi-square fit:")
print("a = {0:8.4f} +/- {1:8.4f}".format(a,sigma))
print("b = {0:8.4f} +/- {1:8.4f}".format(b,sigmb))
print("Chi^2 = {0:8.4f}".format(chi2))

for i in range(1,nfit+1):                               # append model points
   x[n1+i] = x[n+i]
   y[n1+i] = a*x[n+i] + b                        # Chi-square regression line

GraphInit(800,600)

nn[1] = n ; col[1] = "black"; sty[1] =  4                       # data points
nn[2] = n1; col[2] = "red"  ; sty[2] = -1                 # least squares fit
nn[3] = n2; col[3] = "blue" ; sty[3] =  1                    # Chi-square fit
MultiPlot(x,y,sigmy,nn,col,sty,3,10,0.5e0,5.5e0,1,0e0,0e0,0,
          0.15,0.95,0.15,0.85,"x","y","Linear fit")

MainLoop()
