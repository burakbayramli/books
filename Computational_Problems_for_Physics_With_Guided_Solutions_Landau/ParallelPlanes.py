""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work.""" 

# ParallelPlanes.py: Potential q between 2 planes (JDJ 3.20),  e/pi/eps = 1
    
from scipy import special
import numpy as np, matplotlib.pyplot as plt, matplotlib.cm as cm

L = 60;  z0 = 30; m = 400;  n = 400

def potential(r,z):           
    summ = 0                   
    for n in range(1,200):   
        ter = n*np.pi/L         
        s1 = np.sin(ter*z0)    
        s2 = np.sin(ter*z)
        k0 = special.k0(ter*r)  
        term = s1*s2*k0
        summ = summ + term
    return summ  
           
y = np.arange(29,31,1/400.)   
x = np.arange(0.0001, 0.4 ,0.4/400.)  
X, Y = np.meshgrid(x, y) 
Z = potential(X,Y)
plt.figure()
levels = np.arange(20, 180,20)          
CS = plt.contour(Z,levels,linewidths=2, extent=(0.001, 0.4, 29.5, 30.5))
plt.clabel(CS, inline=1, fmt='%4.1f', fontsize=10)
plt.xlabel('r'); plt.ylabel('z')
plt.title("Equipotentials for Charge at z = 30, Plates at z = 0, 60 ")
plt.plot(0.0, z0, 'ro')
plt.show()