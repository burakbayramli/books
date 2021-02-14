""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""


# IntegGauss.py: N point Gaussian quadrature \int_0^1 exp(x) dx
 
from numpy import *
from sys import version
from GaussPoints import GaussPoints

Npts = 10                                            # Numb points
a = 0.; b = 1.                                  # Int ranges
eps=3.E-14                                     # Precision desired
w = zeros( (2001), float)
x = zeros( (2001), float)

def f(x):                                    # Place integrand here
    return (exp(x))                                                         

def GaussInt (Npts, a, b, eps):          # Sum integrand*weight 
    quadra = 0.
    GaussPoints(Npts, a, b, x, w, eps) # Get pts, eps = precison  
    for i in  range(0, Npts):
        quadra += f(x[i])*w[i]           # Sum weighted integrands
    return (quadra)

Ans = GaussInt(Npts, a, b, eps) 
print 
print 'Npts =', Npts, 'Ans =', Ans
print 'eps =',eps,	"Sum-Analytic =", Ans-(exp(1)-1)
