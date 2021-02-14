""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# Simple3Dplot.py: matplotlib 3D plot, rotate & scale wi mouse
			
import matplotlib.pylab  as p                                     
from mpl_toolkits.mplot3d import Axes3D

print "Please be patient while I do importing & plotting"
delta = 0.1
x = p.arange( -3., 3., delta )
y = p.arange( -3., 3., delta )
X, Y = p.meshgrid(x, y)
Z = p.sin(X) * p.cos(Y)                       # Surface height
fig = p.figure()                               # Create figure
ax = Axes3D(fig)                                  # Plots axes
ax.plot_surface(X, Y, Z)                            # Surface
ax.plot_wireframe(X, Y, Z, color = 'r')        # Add wireframe
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
p.show()                                        # Output figure