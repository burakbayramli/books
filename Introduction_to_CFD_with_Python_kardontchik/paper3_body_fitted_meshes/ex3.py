"""
   Usage: run ex3
   one block in Anderson's CFD book (example in Fig 5.5, p 191)
"""
import numpy as np
from numpy import pi as pi
import matplotlib.pylab as plt

from STRUCTUREDMESH import *

plt.close('all')

# physical domain: rectangular grid: [0,N]x[0,M]
N = 50  # N grid points in the x-direction (at constant eta)
M = 20  # M grid points in the y-direction (at constant epsilon)

# box defining the physical domain
x1 = 2.0; x2 = 5.0
y1 = 1.0; y2 = 3.0

# clustering in the x-direction
betax = 6.5
DX = 0.2

# clustering in the y direction
betay = 5.5
DY = 0.4 

f = XY(x1,x2,y1,y2)
x,y = f(N,M,DX=DX,betax=betax,DY=DY,betay=betay)

# visualize

plt.figure(1)
for i in range(M):
    plt.plot(x[:,i],y[:,i],'k')
    plt.hold('on')
for j in range(N):
    plt.plot(x[j,:],y[j,:],'k')
    plt.hold('on')
plt.title('CLUSTERING IN BOTH DIRECTIONS')
plt.axis('equal')
plt.xlabel('x')
plt.ylabel('y')
plt.hold('off')
plt.show()
    
