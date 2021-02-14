""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# GaussPoints.py: N point Gaussian quadrature pts & Wts generation

import math
from numpy import *

def GaussPoints(npts, a, b, x, w, eps):
    m = 0; i = 0; j = 0; t = 0.; t1 = 0.; pp = 0.; p1 = 0.; p2 = 0.; p3 = 0.  
    m = int((npts  +  1)/2 )
    for i in range(1, m + 1):
        t = math.cos(math.pi*(float(i) - 0.25)/(float(npts)  +  0.5) )
        t1 = 1 
        while( (abs(t - t1) ) >=  eps):
            p1 = 1. ;  p2 = 0.  
            for j in range(1, npts + 1):
                p3 = p2;   p2 = p1 
                p1 = ( (2.*float(j) - 1)*t*p2 - (float(j) - 1.)*p3)/(float(j) )
            pp = npts*(t*p1 - p2)/(t*t - 1.) 
            t1 = t
            t = t1  -  p1/pp
        x[i-1] =  - t
        x[npts - i] = t 
        w[i - 1] = 2./( (1. - t*t)*pp*pp) 
        w[npts - i] = w[i - 1]
        
    for j in range(0, npts):
            x[j] = x[j]*(b - a)/2.  +  (b  +  a)/2. 
            w[j] = w[j]*(b - a)/2. 
   
