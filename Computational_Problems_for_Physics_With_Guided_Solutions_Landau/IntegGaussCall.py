""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# IntegGaussCall.py: N point Gaussian quadrature \int[a,b] f(x)dx
 
from numpy import *;  from GaussPoints import GaussPoints

Npts = 10; Ans = 0;  a = 0.;  b = 1.;  eps = 3.E-14
w = zeros(2001, float);  x = zeros(2001, float)       # Arrays

def f(x):  return exp(x)                           # Integrand 

GaussPoints(Npts, a, b, x, w, eps)      #  eps: precison of pts  
for i in  range(0,Npts): Ans += f(x[i])*w[i]   # Sum integrands
print '\n Npts =', Npts, ',   Ans =', Ans
print ' eps =',eps, ', Error =', Ans-(exp(1)-1)