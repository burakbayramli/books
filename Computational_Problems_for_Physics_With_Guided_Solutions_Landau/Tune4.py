""" From "A SURVEY OF COMPUTATIONAL PHYSICS", Python eBook Version
   by RH Landau, MJ Paez, and CC Bordeianu
   Copyright Princeton University Press, Princeton, 2011; Book  Copyright R Landau, 
   Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2011.
   Support by National Science Foundation , Oregon State Univ, Microsoft Corp"""  
	 
# Tune4.py  Model tuning program
	 
import datetime
from numpy import zeros
from math import (sqrt,pow)
from math import pi
from sys import version

if int(version[0])>2: raw_input=input   # raw_input deprecated in Python3
        
Ldim = 200;         iter1 = 0;          step = 0.
ham = zeros( (Ldim, Ldim), float);      diag = zeros( (Ldim), float)
coef = zeros( (Ldim), float);           sigma = zeros( (Ldim), float)
t0 = datetime.datetime.now()                            # Initialize time

for i in range(1, Ldim):                             # Set up Hamiltonian
        for j in range(1, Ldim):
           if abs(j - i) >10: ham[j, i] = 0.  
           else : ham[j, i] = pow(0.3, abs(j - i) )
for i in range(1, Ldim):
    ham[i, i] = i
    coef[i]   = 0.
    diag[i]   = ham[i, i]
coef[1] = 1.;       err = 1.;           iter = 0 ;
print("iter      ener           err ")

while (iter1  < 15 and err > 1.e-6): # Compute current energy & normalize
    iter1 = iter1 + 1
    ener  = 0.
    ovlp1 = 0.
    ovlp2 = 0. 
    for i in range(1, Ldim - 1, 2):
        ovlp1 = ovlp1 + coef[i]   * coef[i]   
        ovlp2 = ovlp2 + coef[i+1] * coef[i+1]    
        t1 = 0.
        t2 = 0. 
        for j in range(1, Ldim):
            t1 = t1 + coef[j] * ham[j, i] 
            t2 = t2 + coef[j] * ham[j, i+1]  
        sigma[i]   = t1
        sigma[i+1] = t2
        ener       = ener + coef[i] * t1 + coef[i+1] * t2 
    ovlp = ovlp1 + ovlp2  
    ener = ener/ovlp 
    fact = 1./sqrt(ovlp) 
    coef[1] = fact*coef[1] 
    err = 0.                                        # Update & error norm
    for i in range(2, Ldim):
        t       = fact*coef[i]
        u       = fact*sigma[i] - ener*t
        step    = u/(ener - diag[i])
        coef[i] = t + step
        err     = err +  step*step 
    err = sqrt(err)   
    print (" %2d  %15.13f  %15.13f "%(iter1, ener, err))
delta_t = datetime.datetime.now()  -  t0                   # Elapsed time
print " time = ",  delta_t
print "press a key to finish"
