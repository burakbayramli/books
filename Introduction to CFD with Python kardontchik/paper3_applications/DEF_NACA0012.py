"""
    DEF_NACA0012
    NACA0012 airfoil in rectangular region x1,y1 = (-4,-4), (x2,y2) = (6,4)
    Uses non-uniform size function
"""
import numpy as np
from numpy import pi as pi
import matplotlib.pylab as plt

from DISTMESH import *

plt.close('all')

# -------------------- AIRFOIL DATA --------------------------
infile = open('NACA0012.dat','r')
# skip the first line (with the title 'NACA 0012' 
infile.readline()       # reads one line and does nothing with it

lines = infile.readlines()
infile.close()

x = []
y = []

for line in lines:
    words = line.split()
    x.append(float(words[0]))
    y.append(float(words[1]))

plt.figure()
plt.plot(x, y, 'k.')
plt.title('NACA0012 AIRFOIL')
plt.grid()
plt.axis('equal')
plt.show()

cverts = np.zeros((len(x),2))
cverts[:,0] = np.asarray(x)
cverts[:,1] = np.asarray(y)

# ------------------- END OF AIRFOIL DATA --------------------

# ----------------------------------------------------------------
#       DISTANCE FUNCTION fd FOR COMPUTATIONAL DOMAIN
# ----------------------------------------------------------------

# Create the shape
# External rectangle
x1 = -4; x2 = 6
y1 = -4; y2 = 4

# define the distance function fd
verts = cverts[0:-1]  # vertices for Polygon (exclude cverts[-1] = cverts[0])
fd_rect = Rectangle(x1,x2,y1,y2)
fd_poly = Polygon(verts)
fd = Diff(fd_rect,fd_poly)
