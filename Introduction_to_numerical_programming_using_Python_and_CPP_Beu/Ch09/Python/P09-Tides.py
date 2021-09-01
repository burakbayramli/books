# Tidal analysis
from math import *
from modfunc import *
from graphlib import *

T1 = 24.8412e0                                                 # lunar period
T2 = 24e0                                                      # solar period
omg1 = 2e0 * pi / (T1 / 2e0)                          # lunar frequency (1/h)
omg2 = 2e0 * pi / (T2 / 2e0)                          # solar frequency (1/h)

def Func1(x, a, npar):                           # model function for MarqFit
   return a[1] + a[2] * cos(omg1 * x) + a[3] * sin(omg1 * x) \
               + a[4] * cos(omg2 * x) + a[5] * sin(omg2 * x)

def Func2(x, func, npar):                       # model function for MultiFit
   func[1] = 1e0
   func[2] = cos(omg1 * x); func[3] = sin(omg1 * x)
   func[4] = cos(omg2 * x); func[5] = sin(omg2 * x)

# main

nn  = [0]*3                                         # ending indexes of plots
col = [""]*3                                                # colors of plots
sty = [0]*3                                                 # styles of plots

inp = open("tides.dat","r")                                  # open data file

line = inp.readline()                # number of observed data and parameters
n = int(line.split()[0])
npar = int(line.split()[1])
nfit = n                                # number of points plotted from model
                                                            # allocate arrays
x     = [0]*(n+nfit+1)                       # x-coordinates of observed data
y     = [0]*(n+nfit+1)                       # y-coordinates of observed data
sigmy = [0]*(n+1)                      # standard deviations of observed data
func  = [0]*(npar+1)                              # values of basis functions
a     = [0]*(npar+1)                                       # model parameters
sigma = [0]*(npar+1)                            # uncertainties of parameters

iopt = int(inp.readline())               # initialization option for sigmy[i]
line = inp.readline()
for i in range(1,npar+1): a[i] = float(line.split()[i-1]) # parameter guesses
for i in range(1,n+1):
   line = inp.readline()
   x[i] = float(line.split()[0])                              # observed data
   y[i] = float(line.split()[1])
   if (iopt): sigmy[i] = float(line.split()[2])                 # uncertainty
inp.close()

GraphInit(1200,800)

nn[1] = n       ; col[1] = "red" ; sty[1] = 1                 # observed data
nn[2] = n + nfit; col[2] = "blue"; sty[2] = 1                  # fitted model

#----------------------------------------------------- Levenberg-Marquart fit
chi2 = MarqFit(x,y,sigmy,n,iopt,a,sigma,npar,Func1)

print("Levenberg-Marquardt fit\n")
for i in range(1,npar+1):
   print(("a[{0:d}] = {1:6.3f}  sigma[{2:d}] = {3:7.1e}").
         format(i,a[i],i,sigma[i]))

h = (x[n]-x[1])/(nfit-1)
for i in range(1,nfit+1):                               # append model points
   x[n+i] = x[1] + (i-1)*h
   y[n+i] = Func1(x[n+i],a,npar)

MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,0.10,0.95,0.60,0.90,
          "t (h)","z (m)","Tidal analysis - Levenberg-Marquardt fit")

#------------------------------------------------------------ Multilinear fit
chi2 = MultiFit(x,y,sigmy,n,iopt,a,sigma,npar,Func2)

print("\nMultilinear fit\n")
for i in range(1,npar+1):
   print(("a[{0:d}] = {1:6.3f}  sigma[{2:d}] = {3:7.1e}").
         format(i,a[i],i,sigma[i]))

h = (x[n]-x[1])/(nfit-1)
for i in range(1,nfit+1):                               # append model points
   xi = x[1] + (i-1)*h
   Func2(xi,func,npar)                             # evaluate basis functions
   f = 0e0
   for j in range(1,npar+1): f += a[j]*func[j]               # evaluate model
   x[n+i] = xi; y[n+i] = f

MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,0.10,0.95,0.10,0.40,
          "t (h)","z (m)","Tidal analysis - Multilinear fit")

MainLoop()
