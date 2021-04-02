"""
   Usage: run toro_explosion
      Explosion test, Toro p587
      solve Euler equations using the HLLC solver and ideal triangular mesh
"""
import numpy as np
from numpy import pi as pi
import matplotlib.pylab as plt
from time import clock

from DISTMESH import *
from Meshing_Tools import *
from VISUAL_TRI import *

from MESH import *
from IC_conditions import *

from VISUAL_3D import *

from HLLC_FLUX import *
from HLLC_SOLVER_TRI import *

plt.close('all')

# -----------------------------------------------------------
#           case 18: INITIAL CONDITIONS: Toro, Eq 17.3, p587
#           tuples = (p,d,u,v)
# -----------------------------------------------------------
R = 0.4   # radius separating the in and out regions
inside = [1.0, 1.0, 0.0, 0.0]
outside = [0.1, 0.125, 0.0, 0.0]

# simulation time and Courant number for stability
sim_time = 0.25  # simulation time
CFL = 0.1 # Courant number

# -----------------------------------------------------------
#               DOMAIN AND GRID GRANULARITY
# -----------------------------------------------------------
# physical domain
x1 = -1; x2 = 1; y1 = -1; y2 = 1

# h0 defines the grid granularity. For final simulations use h0 = 0.02
# (it generates about 22,000 triangles, corresponding to a resolution
# of about 0.01 in the x- and y- directions.
# Use also Iplot = 200 (to avoid cluttering of the desktop screen)
# CPU time = 40 minutes, number of iterations = 424

# for initial testing use a larger h0, for ex, h0 = 0.08 and Iplot = 40

h0 = 0.08 # 0.02  # note: x2 - x1 = 2.0 here

# -----------------------------------------------------------
#                           MESH
# -----------------------------------------------------------

mesh = MESH(x1,x2,y1,y2,h0)
tri,p,neighbors,matched_pairs,area,e01,e12,e20,ang01,ang12,ang20 = mesh()


# map the initial pressure, density and speeds into the physical mesh
# based on the Initial Conditions specification:
pp,dd,uu,vv = ic_phys_toro_explosion(inside,outside,R,p,tri)    

start_time = clock()

solver = HLLC_TRI(CFL,pp,dd,uu,vv, \
                 area,e01,e12,e20,ang01,ang12,ang20, \
                 neighbors,matched_pairs,p,tri,x1,x2,y1,y2,Iplot=40)

dd,uu,vv,pp,ee = solver(sim_time)

end_time = clock()
print 'CPU time = %g [sec]' % (end_time - start_time)

# ------------------------------------------------------
#               VISUALIZATION
# ------------------------------------------------------
vis = VISUAL_2D(x1,x2,y1,y2,sim_time,p,tri,pp,dd,uu,vv)
vis()
vis3D = VISUAL_3D(x1,x2,y1,y2,p,tri)
vis3D(pp,0)
vis3D(dd,1)
