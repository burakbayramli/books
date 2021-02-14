""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# Spline.py  Spline fit with slide to control number of points

from visual import *
from visual.graph import *

x = array([0., 0.1, 0.2, 0.37, 0.5, 0.55, 0.65, 0.77, 0.9])     # Data
y = array([10., 16., 45., 83., 92., 109., 99., 109., 4.])
n = 9;                                           # N points in table
Nfit = 15;                                       # N of interpolations

y2 = zeros((n), float);    u = zeros((n), float)    # Initialize

for i in range(0, n):                  
    yp1 = (y[1]-y[0])/(x[1]-x[0])-(y[2]-y[1])/(x[2]-x[1])+(y[2]-y[0])/(x[2]-x[0])

ypn = (y[n-1] - y[n-2])/(x[n-1] - x[n-2]) - (y[n-2] - y[n-3])/(x[n-2] 
             - x[n-3]) + (y[n-1] - y[n-3])/(x[n - 1] - x[n - 3])
if (yp1 > 0.99e30):
    y2[0] = 0.
    u[0] = 0.
else:
    y2[0] = - 0.5
    u[0]  = (3./(x[1] - x[0]) )*( (y[1] - y[0])/(x[1] - x[0]) - yp1)

for i in range(1, n - 1):                            # Decomposition loop
    sig   = (x[i] - x[i - 1])/(x[i + 1] - x[i - 1]) 
    p     = sig*y2[i - 1] + 2. 
    y2[i] = (sig - 1.)/p 
    u[i]  = (y[i+1] - y[i])/(x[i+1] - x[i]) - (y[i]-y[i-1])/(x[i]-x[i-1])
    u[i]  = (6.*u[i]/(x[i + 1] - x[i - 1]) - sig*u[i - 1])/p

if (ypn > 0.99e30):  qn = un = 0.                      # Test for natural
else:
    qn = 0.5;
    un = (3/(x[n-1] - x[n-2]) )*(ypn - (y[n-1] - y[n-2])/(x[n-1]-x[n-2]))
y2[n-1] = (un - qn*u[n-2])/(qn*y2[n-2] + 1.)

for k in range(n-2, 1, - 1):  y2[k] = y2[k]*y2[k + 1] + u[k]

for i in range(1, Nfit + 2):                                 # Begin fit
    xout = x[0] + (x[n - 1] - x[0])*(i - 1)/(Nfit) 
    klo = 0;    khi = n - 1                             # Bisection algor
    while (khi - klo >1):
        k = (khi + klo) >> 1
        if (x[k] > xout): khi  = k
        else: klo = k
    h = x[khi] - x[klo] 
    if (x[k] > xout):  khi = k
    else: klo = k 
    h = x[khi] - x[klo]
    a = (x[khi] - xout)/h 
    b = (xout - x[klo])/h 
    yout = a*y[klo]+b*y[khi] +((a*a*a-a)*y2[klo]+(b*b*b-b)*y2[khi])*h*h/6
    print("xout, yout = ", xout,",", yout)
