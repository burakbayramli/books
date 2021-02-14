""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# CoulWF.py:  Regular Coulomb scattering wave function

from scipy import special; from mpmath import * # hypergeometric
import matplotlib.pyplot as plt, numpy as np
from math import *

#     Initializations
f1 = np.zeros((10),complex); Rea = np.zeros((10,161),float)
zi = complex(0,1.0)
mAu =  196.966569*931.494;  mAlpha =  4.002602*931.494
Zau = 79;  Zalph = 2                                        
mu = mAlpha*mAu/(mAlpha + mAu)                                                                       
hbarc = 197.33                   # MeV-fm, E in MeV, r in fm 
Elab =  7.                                              
Ecom = Elab*mAu/(mAlpha + mAu)                
vel = sqrt(Ecom*2/mu)                                 
ka = sqrt(2.0*mu*Ecom)/hbarc                                   
etaco = Zalph*Zau*mu/(hbarc*ka*137.)        # Coulomb parameter
expi = exp(-0.5*etaco*pi)  
    
i = 0                       # Main loop over r and i            
for r in np.arange(0.1,80.5,0.5):     
    rho = complex(0,-2*ka*r)                     # -2ikr
    expo = complex(cos(ka*r),sin(ka*r))        # exp(ikr)
    for L in range(0,10):             
        a = L + 1.0 + etaco * zi    # Arg gamma function
        sol = hyp1f1(a, 2*L+2., rho)    # Hypergeometric 
        rhoL = (-rho)**L               
        gam = special.gamma(a)           # Gamma(l+1+in)
        upar = rhoL*expo *sol*gam*expi/factorial(2*L)
        f1[L] = upar/sqrt(vel)
        Rea[L,i] = f1.real[L]                 # Real psi
    i += 1
    
rr = np.arange(0.1,80.5,0.5)
plt.plot(rr,Rea[0,:],label = 'S')
plt.plot(rr,Rea[1,:],label = 'P',linewidth=2)
plt.plot(rr,Rea[2,:],label = 'D',linewidth=3)
plt.legend()
plt.xlabel("r (fermis)")
plt.title ("Radial Coulmb Wave Functions $y_l(r)$ for $l = 0,1,2$")
plt.show()