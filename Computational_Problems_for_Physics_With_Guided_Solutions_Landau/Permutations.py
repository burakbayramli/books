""" From "ComboPUTATIONAL PHYSICS" & "ComboPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# Permutations.py accessible states versus energy

import matplotlib.pyplot as plt, numpy as np
from math import *

k = 0;   n = 25;  B = 1.;  mu = 1.;  i = 0;   Eo = -mu*B*n
Energy = [0.]*(13); Combo = [0.]*(13)  
for k in range(0,26):
    c = factorial(n)/(factorial(n-k)*factorial(k)) 
    E =  -(n-k)*mu*B + k*mu*B
    print(k, E-Eo,c)
    if k < 13:          # Plot only first half (symmetry)
        Energy[i] = E - Eo  # 
        Combo[i] = c            
        i += 1 
plt.subplot(121)            # L: accessible states vs E-Eo
plt.plot(Energy,Combo)
plt.title('Number vs E-Eo')
plt.subplot(122)
plt.loglog(Energy, Combo)      
plt.title('log(Number) vs log(E-Eo)')
plt.show()
