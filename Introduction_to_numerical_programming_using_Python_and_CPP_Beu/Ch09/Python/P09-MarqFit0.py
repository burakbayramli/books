# Non-linear fit by the Levenberg-Marquardt method
from math import *
from modfunc import *

def Func(x, a, npar):                                        # model function
   return a[1] * exp(-pow(x-a[2],2)/a[3])

# main

inp = open("fit.dat","r")                                    # open data file

line = inp.readline()                # number of observed data and parameters
n = int(line.split()[0])
npar = int(line.split()[1])
                                                            # allocate arrays
x     = [0]*(n+1)                            # x-coordinates of observed data
y     = [0]*(n+1)                            # y-coordinates of observed data
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

out = open("fit.txt","w")                                  # open output file
out.write(("n = {0:2d}   npar = {1:2d}\n").format(n,npar))
out.write("Initial parameters:\n")
for i in range(1,npar+1): out.write(("a[{0:d}] = {1:6.3f}\n").format(i,a[i]))

chi2 = MarqFit(x,y,sigmy,n,iopt,a,sigma,npar,Func)   # Levenberg-Marquart fit

out.write("\nFinal parameters:\n")
for i in range(1,npar+1):
   out.write(("a[{0:d}] = {1:6.3f}  sigma[{2:d}] = {3:7.1e}\n").
             format(i,a[i],i,sigma[i]))
out.write("\nChi^2 = {0:7.1e}\n".format(chi2))
out.write("\n i      x         y       sigma      yfit     y-yfit\n")
for i in range(1,n+1):
   yfit = Func(x[i],a,npar)            # model values for adjusted parameters
   out.write(("{0:2d}{1:10.5f}{2:10.5f}{3:10.5f}{4:10.5f}{5:10.1e}\n").
             format(i,x[i],y[i],sigmy[i],yfit,y[i]-yfit))
out.close()
