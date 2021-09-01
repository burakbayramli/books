# 3D rendering of a molecule
from graphlib import *

n = 5                                                   # number of particles
x = [0, 0.00, 0.52, 0.52,-1.04, 0.00]                           # coordinates
y = [0, 0.00, 0.90,-0.90, 0.00, 0.00]                          # [0] not used
z = [0, 0.00,-0.37,-0.37,-0.37, 1.10]
r = [0, 0.30, 0.20, 0.20, 0.20, 0.20]                                 # radii
col = ["", "red", "blue", "blue", "blue", "blue"]                    # colors

dmax = 1.5e0                                      # cutoff distance for bonds

GraphInit(600,600)                                            # create canvas

PlotParticles(x,y,z,r,col,n,dmax,0e0,0e0,0,0e0,0e0,0,        # plot particles
              0.15,0.85,0.15,0.85,"CH4 molecule")

MainLoop()                                         # enter Tkinter event loop
