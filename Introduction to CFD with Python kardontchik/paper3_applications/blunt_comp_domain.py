"""
   Usage: run blunt_comp_domain
   (for Cubic Cylinder simulations)
"""
import numpy as np
from numpy import pi as pi
import matplotlib.pylab as plt

plt.close('all')


# -------------------------------------------------
#               Cubic Cylinder
# -------------------------------------------------
a = 0.427
   
N = 81  # number of vertices
t = np.linspace(-2.5,2.5,N)
verts = np.zeros((N,2))
verts[:,0] = a*(np.abs(t))**3 - 1.0
verts[:,1] = t

# -------------------------------------------------
#           computational domain
# -------------------------------------------------
# rectangle
x1 = -2.0; x2 = 2.0; y1 = 0; y2 = 2.4
x = np.array([-2, 2, 2, -2, -2])
y = np.array([ 0, 0, 2.4, 2.4, 0])

plt.figure(1)
plt.plot(verts[:,0], verts[:,1],'k',x,y,'r')
plt.grid()
plt.title('CUBIC CYLINDER AND COMPUTATIONAL DOMAIN (RED)')
plt.axis('equal')
plt.xlabel('X')
plt.ylabel('Y')
plt.show()
