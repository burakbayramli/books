# -*- coding: utf-8 -*-
"""
Created on Mon Dec 12 04:29:18 2016

@author: mpaez
"""
from scipy import special
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm

L=60                  # close to 20*pi, sistance between plates
z0=30                 # position of charge L/2
m=400                 # for positions of z, and r
n=400
#take constant e/(pi eps0L)=1
 
 def potential(r,z):          # Finds the potential at point (r,z)
    summ=0                   # to sum product
    for n in range(1,200):  # number of terms to find  potential
        ter=n*np.pi/L        # common term in arguments of functions
        s1=np.sin(ter*z0)    
        s2=np.sin(ter*z)
        k0=special.k0(ter*r) #  k0 Modified Bessel Function
        term=s1*s2*k0
        summ=summ+term
    return summ             # value of potential at (r,z)
    
y = np.arange(29.5,30.5,1/400.)     #value of z interesting region   
x = np.arange(0.0001,0.4,0.4/400.) # values of r that are interesting
X, Y = np.meshgrid(x, y) 
Z=potential(X,Y)                     #  potential for all values of r and  z
plt.figure()                         # to plot the figure
levels = np.arange(05, 180,20)         # range of levels to plot every 2
CS = plt.contour(Z,levels,linewidths=2, extent=(0.001, 0.4, 0, 60))
plt.clabel(CS, inline=1, fmt='%4.1f', fontsize=10)
plt.xlabel('Rho (Distance from z Axis)')
plt.ylabel('Z (Distance Between Plates)')
plt.title("Equipotentia Sufaces, Charge at z = 30, Plates at 0, 60")
plt.plot(0.0,z0,'ro')
plt.show()

        
        
    
