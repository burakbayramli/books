# Multilinear fit
from math import *
from modfunc import *
from graphlib import *

def Func(x, func, npar):      # returns the basis functions sin(x) and cos(x)
   func[1] = 1e0
   func[2] = cos(x);   func[3] = sin(x)
   func[4] = cos(2*x); func[5] = sin(2*x)

# main

nn  = [0]*3                                      # end-indexes of the 3 plots
col = [""]*3                                                # colors of plots
sty = [0]*3                                                 # styles of plots

n = 9                                               # number of observed data
npar = 5                                         # number of model parameters
nfit = 100                              # number of points plotted from model

x     = [0]*(n+nfit+1)                       # x-coordinates of observed data
y     = [0]*(n+nfit+1)                       # y-coordinates of observed data
sigmy = [0]*(n+1)                      # standard deviations of observed data
func  = [0]*(npar+1)                              # values of basis functions
a     = [0]*(npar+1)                                       # model parameters
sigma = [0]*(npar+1)                            # uncertainties of parameters

                  # observed data generated from f(x) = 1 - 2sin(x) + cos(2x)
x[1] = 0.000; y[1] =  2.000; x[2] = 0.785; y[2] = -0.414
x[3] = 1.571; y[3] = -2.000; x[4] = 2.356; y[4] = -0.414
x[5] = 3.142; y[5] =  2.000; x[6] = 3.927; y[6] =  2.414
x[7] = 4.712; y[7] =  2.000; x[8] = 5.498; y[8] =  2.414
x[9] = 6.283; y[9] =  2.000

iopt = 0                              # least squares fit: equal errors sigmy
chi2 = MultiFit(x,y,sigmy,n,iopt,a,sigma,npar,Func)

print("Multilinear fit:")
for i in range(1,npar+1):
   print(("a[{0:d}] = {1:8.4f} +/- {2:8.4f}").format(i,a[i],sqrt(sigma[i])))
print("Chi^2 = {0:8.4f}".format(chi2))

h = (x[n]-x[1])/(nfit-1)
for i in range(1,nfit+1):                               # append model points
   xi = x[1] + (i-1)*h
   Func(xi,func,npar)                              # evaluate basis functions
   f = 0e0
   for j in range(1,npar+1): f += a[j]*func[j]               # evaluate model
   x[n+i] = xi; y[n+i] = f

GraphInit(800,600)

nn[1] = n       ; col[1] = "red" ; sty[1] = 0                 # observed data
nn[2] = n + nfit; col[2] = "blue"; sty[2] = 1                  # fitted model

MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
          0.15,0.95,0.15,0.85,"x","y","Multilinear fit")

MainLoop()
