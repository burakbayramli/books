# Absorption maximum of halides by linear and non-linear regression
from modfunc import *
from graphlib import *

def Func(x, a, npar):                            # model function for MarqFit
   return a[1] * pow(x,a[2])

# main

nfit = 100                              # number of points plotted from model
nn  = [0]*3                                         # ending indexes of plots
col = [""]*3                                                # colors of plots
sty = [0]*3                                                 # styles of plots

inp = open("halides.dat","r")                                # open data file

line = inp.readline()                # number of observed data and parameters
n = int(line.split()[0])
npar = int(line.split()[1])
                                                            # allocate arrays
x     = [0]*(n+nfit+1)                       # x-coordinates of observed data
y     = [0]*(n+nfit+1)                       # y-coordinates of observed data
sigmy = [0]*(n+1)                      # standard deviations of observed data
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

GraphInit(1200,600)

nn[1] = n       ; col[1] = "red" ; sty[1] = 4                 # observed data
nn[2] = n + nfit; col[2] = "blue"; sty[2] = 1                  # fitted model

#----------------------------------------------------- Levenberg-Marquart fit
a[1] = 1e3; a[2] = 1e0                           # guess for model parameters
chi2 = MarqFit(x,y,sigmy,n,iopt,a,sigma,npar,Func)

print("Levenberg-Marquardt fit\n")
print("a = {0:6.2f} +/- {1:6.2f}".format(a[1],sigma[1]))
print("b = {0:6.2f} +/- {1:6.2f}".format(a[2],sigma[2]))
print("Chi^2 = {0:6.2f}".format(chi2))

h = (x[n]-x[1])/(nfit-1)
for i in range(1,nfit+1):                               # append model points
   x[n+i] = x[1] + (i-1)*h
   y[n+i] = Func(x[n+i],a,npar)                              # evaluate model

MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
          0.10,0.45,0.15,0.85,"d (A)","lam (A)",
          "Absorption maximum of halides - Levenberg-Marquardt fit")

#----------------------------------------------------------------- Linear fit
for i in range(1,n+1): sigmy[i] = log(1e0+sigmy[i]/y[i])     # transform data
for i in range(1,n+nfit+1): x[i] = log(x[i]); y[i] = log(y[i])

(a[1], a[2], sigma[1], sigma[2], chi2) = LinFit(x,y,sigmy,n,iopt)

print("\nLinear fit\n")
print("a = {0:6.2f} +/- {1:6.2f}".format(exp(a[2]),exp(sigma[2])))
print("b = {0:6.2f} +/- {1:6.2f}".format(a[1],sigma[1]))

for i in range(1,nfit+1): y[n+i] = a[1] * x[n+i] + a[2]      # evaluate model

MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
          0.60,0.95,0.15,0.85,"log(d)","log(lam)",
          "Absorption maximum of halides - Linear fit")

MainLoop()
