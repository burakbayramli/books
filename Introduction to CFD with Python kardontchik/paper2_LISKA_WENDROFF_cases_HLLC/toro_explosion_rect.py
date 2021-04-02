"""
   Usage: run toro_explosion_rect
      solve Euler equations with HLLC solver and
      ideal (rectangular) triangular mesh
"""
import numpy as np
from numpy import pi as pi
import matplotlib.pylab as plt
from time import clock

from CREATE_IDEAL_MESH import *
from Meshing_Tools import *
from VISUAL_TRI import *

from VISUAL_3D import *

from IC_conditions import *

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
CFL = 0.1  # Courant number

# -----------------------------------------------------------
#               DOMAIN AND GRID GRANULARITY
# -----------------------------------------------------------
# physical domain
x1 = -1; x2 = 1
y1 = -1; y2 = 1

h0 = 0.02

# For toro explosion test:
# Using h0 = 0.02,
# a) it generates about 22,000 triangles in the computational domain)
#    corresponding to a resolution of about 0.01 in the x- and y-directions
# b) CPU time of 0.5 hours, # iterations = 371
# c) use Iplot = 200 (to avoid screen cluttering)

# for fast initial testing use a larger granulariy, for ex, h0 = 0.08
# (and, say, Iplot = 10)

# -----------------------------------------------------------
#               CREATE THE MESH AND THE BOUNDARY
# -----------------------------------------------------------
dx = h0; dy = h0

create_mesh = CREATE_IDEAL_MESH_RECT(x1,x2,y1,y2,dx,dy)
p,tri,bbars,boundary_nodes,boundary_bars = create_mesh()

# ----------------------------------------------------------
#       CREATE GEOMETRY INFO, GHOSTS FOR BC and NEIGHBORS INFO
# ----------------------------------------------------------
geo = GEOMETRY(p,tri)
area, e01,e12,e20,ang01,ang12,ang20 = geo()

gboundary = GHOSTS(p,tri,bbars,e01,e12,e20,ang01,ang12,ang20)
btri,tghost,comp_p,comp_tri = gboundary()

# Notice that the node array 'comp_p' now includes also the additional
# nodes of the ghost triangles added to the boundary

matched_pairs = matching_pairs(bbars,comp_tri)
# Note: len(matched_pairs) = len(bbars) = len(tghost)
#    if matched_pairs[i] = [43,12], this means that the
#    comp_tri[43] is the ghost triangle abutted to the
#    comp_tri[12] triangle inside the physical space

neighbors = vecinos(bbars,comp_tri)
# Note: for each triangle in the physical domain it gives the
#       three neighbors (triangles) abutted to it

plot_ghosts(comp_p,tghost)

# -------------------------------------------------------------
#       APPLY IC ON THE MESH
# -------------------------------------------------------------

# map the initial pressure, density and speeds into the physical mesh
# based on the Initial Conditions specification:
pp,dd,uu,vv = ic_phys_toro_explosion(inside,outside,R,p,tri)

start_time = clock()

solver = HLLC_TRI(CFL,pp,dd,uu,vv, \
                 area,e01,e12,e20,ang01,ang12,ang20, \
                 neighbors,matched_pairs,p,tri,x1,x2,y1,y2,Iplot=200)

dd,uu,vv,pp,ee = solver(sim_time)

end_time = clock()
print 'CPU time = %g [sec]' % (end_time - start_time)

# ------------------------------------------------------
#               VISUALIZATION
# ------------------------------------------------------
# visualization domain for final plot
vx1 = -1; vx2 = 1
vy1 = -1; vy2 = 1

vis = VISUAL_2D(vx1,vx2,vy1,vy2,sim_time,p,tri,pp,dd,uu,vv)
vis()

vis3D = VISUAL_3D(x1,x2,y1,y2,p,tri)
vis3D(pp,0)
vis3D(dd,1)
