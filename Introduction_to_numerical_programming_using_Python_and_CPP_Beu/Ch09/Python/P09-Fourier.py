# Fourier analysis using multilinear fit
from math import *
from modfunc import *
from graphlib import *

def Func0(x, func, npar):                              # sine basis functions
   for i in range(1,npar+1): func[i] = sin(i*x)


def Func(x, func, npar):                            # Fourier basis functions
   cosx = cos(x); cos1 = 1e0                                     # npar = odd
   sinx = sin(x); sin1 = 0e0

   func[1] = 1e0
   for i in range(2,npar,2):
      cosi = cosx * cos1 - sinx * sin1
      sini = sinx * cos1 + cosx * sin1
      cos1 = cosi; sin1 = sini

      func[i] = cosi; func[i+1] = sini

# main

nn  = [0]*3                                      # end-indexes of the 3 plots
col = [""]*3                                                # colors of plots
sty = [0]*3                                                 # styles of plots

n = 500                                             # number of observed data
npar = 51                                        # number of model parameters
nfit = n                                # number of points plotted from model

x     = [0]*(n+nfit+1)                       # x-coordinates of observed data
y     = [0]*(n+nfit+1)                       # y-coordinates of observed data
sigmy = [0]*(n+1)                      # standard deviations of observed data
func  = [0]*(npar+1)                              # values of basis functions
a     = [0]*(npar+1)                                       # model parameters
sigma = [0]*(npar+1)                            # uncertainties of parameters

xmin = -pi; xmax = pi                                  # generate data points
h = (xmax-xmin)/(n-1)
for i in range(1,n+1):
   x[i] = xmin + (i-1)*h
   y[i] = -1e0 if x[i] < 0e0 else 1e0                # periodic step function
#  y[i] = x[i]                                          # "sawtooth" function
#  y[i] = x[i] - pi/2                           # shifted "sawtooth" function

iopt = 0                              # least squares fit: equal errors sigmy
chi2 = MultiFit(x,y,sigmy,n,iopt,a,sigma,npar,Func0)

h = (x[n]-x[1])/(nfit-1)
for i in range(1,nfit+1):                               # append model points
   xi = x[1] + (i-1)*h
   Func0(xi,func,npar)                             # evaluate basis functions
   f = 0e0
   for j in range(1,npar+1): f += a[j]*func[j]               # evaluate model
   x[n+i] = xi; y[n+i] = f

GraphInit(1200,600)

nn[1] = n       ; col[1] = "red" ; sty[1] = 1                 # observed data
nn[2] = n + nfit; col[2] = "blue"; sty[2] = 1                  # fitted model

MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
           0.10,0.45,0.15,0.85,"x","y","Multilinear fit")

for i in range(1,npar+1): x[i] = float(i)
Plot(x,a,npar,"red",3,0.60,0.95,0.15,0.85,"n","a","Fourier coefficients")

MainLoop()
