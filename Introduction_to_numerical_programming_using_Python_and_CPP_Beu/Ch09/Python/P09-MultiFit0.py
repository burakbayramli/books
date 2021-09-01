# Multilinear fit
from math import *
from modfunc import *

def Func(x, func, npar):
   func[1] = sin(x)           # returns the basis functions sin(x) and cos(x)
   func[2] = cos(x)

# main

n = 5                                               # number of observed data
npar = 2                                         # number of model parameters

x     = [0]*(n+1)                            # x-coordinates of observed data
y     = [0]*(n+1)                            # y-coordinates of observed data
sigmy = [0]*(n+1)                      # standard deviations of observed data
func  = [0]*(npar+1)                              # values of basis functions
a     = [0]*(npar+1)                                       # model parameters
sigma = [0]*(npar+1)                            # uncertainties of parameters

x[1] =  0e000; y[1] =  1e000                  # observed data generated from:
x[2] =  0.785; y[2] =  1.414                         # f(x) = sin(x) + cos(x)
x[3] =  1.571; y[3] =  1e000
x[4] =  2.356; y[4] =  0e000
x[5] =  3.141; y[5] = -1e000

iopt = 0                              # least squares fit: equal errors sigmy
chi2 = MultiFit(x,y,sigmy,n,iopt,a,sigma,npar,Func)

print("Multilinear fit:")
for i in range(1,npar+1):
   print(("a[{0:d}] = {1:8.4f} +/- {2:8.4f}").format(i,a[i],sqrt(sigma[i])))
print("Chi^2 = {0:8.4f}".format(chi2))
