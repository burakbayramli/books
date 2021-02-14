""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""


# IntegGauss.py: N point Gaussian quadrature \int_0^1 exp(x) dx
 
from numpy import *
from sys import version
from GaussPoints import GaussPoints

Npts = 10                               # Numb points
Xmin = 0.; Xmax = 1.                    # Int ranges
eps=3.E-14                              # Precision desired
w = zeros( (2001), float)
x = zeros( (2001), float)

def f(x):              # Place integrand here
    return (exp(x))                                                         

def gaussint (Npts, min, max, eps):      # Sum integrand*weight 
    quadra = 0.
    GaussPoints(Npts, min, max, x, w, eps) # Get pts&wts, eps=precison  
    for i in  range(0, Npts):
        quadra   += f(x[i])*w[i] # Sum weighted integrands
    return (quadra)

Ans = gaussint(Npts, Xmin, Xmax, eps) 
print (" N = ", Npts, " Ans = ", Ans, " eps = ",eps,\
	" err = ", Ans-(exp(1)-1))
