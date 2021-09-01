# 3D rendering of an airplane
from graphlib import *

n = 10                                                      # number of nodes
x = [0, -2.7,-0.7,-4.6, 1.8, 0.8, 8.0, 2.2, 1.7,-2.7, 2.7]      # coordinates
y = [0, -0.8,-5.1, 3.6,-0.7, 1.5, 2.4, 0.6, 1.7, 1.5, 1.4]     # [0] not used
z = [0,  0.5, 3.6,-2.5, 0.5,-1.0,-1.7, 0.8, 0.0, 3.8, 0.3]

n3 = 9                                         # number of defining triangles
ind1 = [0, 1, 1, 1, 1, 1, 1, 1, 6, 1]           # indexes of triangle corners
ind2 = [0, 2, 3, 4, 5, 4, 5, 7, 7, 9]
ind3 = [0, 4, 5, 6, 6, 7, 8, 8, 8,10]

GraphInit(600,600)                                            # create canvas

PlotStruct(x,y,z,n,ind1,ind2,ind3,n3,0e0,0e0,0,0e0,0e0,0,    # plot structure
           0.15,0.85,0.15,0.85,"Airplane")

MainLoop()                                         # enter Tkinter event loop
