""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
   
# LaplaceDisk.py:  Laplace's eqtn in cyclinder, Matplotlib 3D plot 

from scipy import special; from mpmath import *      # for hypergeo 
from matplotlib.pyplot import figure, show, rc; import numpy as np

V = pi;  R = 1;  r = 1.1;  const = 2*V*R/(pi*r);  st = [1,1]                          
incr = 2*pi/100.;  thView = [pi/2, 3*pi/2];  Mmax = 10                
x = [0]*(100);  xx = [0]*(100);  xxx = [0]*(100)  # Declare arrays

def legendre(n,x):                           # Legendre polymonial  
    if(n==0):    p = 1          
    elif(n==1):  p = x          
    else:
        p0 = 1;  p1 = x
        for m in range(1,n):
            p2 = (((2*m+1)*x*p1-m*p0)/(m+1))        # Recurrence
            p0 = p1;  p1 = p2;  p = p2
    return p
def U(r,theta):   
    summ = 0
    for m in range(0,Mmax):
        twom = 2*m;  w = cos(theta)
        leg = legendre(twom,w)
        term = ((-1)**m) *(R/r)**twom*leg/(twom+1)
        summ = summ + term
    pot = summ*const             
    return pot
    
i = 0    
for theta in np.arange(0,2*pi,incr):
    x[i] = U(r,theta);  xx[i] = U(3,theta); xxx[i] = U(1.3,theta)        
    i += 1
    
fig = figure()
ax = fig.add_subplot(111, projection = 'polar')
theta =  np.arange(0,2*pi,incr)
ax.plot(theta,x,label='r = 1.1',linewidth=3)
ax.plot(theta,xx,color='r',label='r = 3',linewidth=3)
ax.plot(theta,xxx,linewidth=3,label='r = 1.3',color=(1,0.8,0))
ax.plot(thView,st,linewidth=6,color=(0.4,0.4,0.4),label="Disk Edge") 
ax.legend(loc='best')
show()
        