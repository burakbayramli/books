# Linear fit of a model to observed data points
from modfunc import *

# main

n = 5                                               # number of observed data

x = [0]*(n+1); y = [0]*(n+1)                                  # observed data
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

for i in range(1,n+1): sigmy[i] = 0.15*y[i]    # generate standard deviations

iopt = 1                             # Chi-square fit: different errors sigmy
(a, b, sigma, sigmb, chi2) = LinFit(x,y,sigmy,n,iopt)

print("\nChi-square fit:")
print("a = {0:8.4f} +/- {1:8.4f}".format(a,sigma))
print("b = {0:8.4f} +/- {1:8.4f}".format(b,sigmb))
print("Chi^2 = {0:8.4f}".format(chi2))
