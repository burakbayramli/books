"""
   Usage: run toro_wedge
      Toro, wedge test, p 591
      solve Euler equations using the HLLC solver and ideal triangular mesh
"""
import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay
import matplotlib.pylab as plt
from time import clock

from DISTMESH import *
from Meshing_Tools import *
from VISUAL_TRI_GENERAL import *

from BOUNDARY_PROCESSING_TRI import *
from IC_conditions import *
from Impose_BC import *

from HLLC_FLUX import *
from RK_HLLC_SOLVER_TRI_GENERAL import *

plt.close('all')

# -----------------------------------------------------------
#           INITIAL CONDITIONS: Toro, Eq 17.3, p587
#               tuples= (p,d,u,v)
# -----------------------------------------------------------
# Initial Data:
p_Right = 1.01325e5     # 1 atm
d_Right = 1.225         # kg/m^3
u_Right = 0
v_Right = 0
MS = 1.7                # shock wave Mach number

# initial position of the shock wave
x0 = 4.0

# simulation time and Courant number for stability
sim_time = 0.027  # simulation time
CFL = 0.8 # Courant number
# CPU = 26.8 min for sim_time = 0.027 and CFL=0.8

# ----------------------------------------------------------
#                   PHYSICS
# ----------------------------------------------------------

# find initial states to the Left and Right of the shock
Left, Right = find_wedge_states(p_Right,d_Right,u_Right,v_Right,MS)
# Left = [p_Left,d_Left,u_Left,v_Left]
# Right = [p_Right,d_Right,u_Right,v_Right]

# ---------------------------------------------------------
#           Define the Computational Domain
#    (create a polygon by defining its vertices)
# ---------------------------------------------------------

# polygon vertices
v0 = [0.0,0.0]
v1 = [4.69, 0.0]
phi = (pi/180.0)*25.0
v2 = [25.0, (25.4 - 4.69)*np.tan(phi)]
v3 = [25.0, 16.5]
v4 = [0, 16.5]

verts = np.array([ v0,v1,v2,v3,v4 ])

# --------------------------------------------------------------
#                       DEFINE BC
# --------------------------------------------------------------
# For each segment of the boundary polygon define the type of BC:
#       transmissive = 0 (for ex, outflow conditions)
#       reflective = 1 (for ex, inviscid fluid at walls)
#       forced = 2 (for ex, inflow conditions)
#
# Walking around the perimeter of the polygon such that the physical
# domain is always at your left (counterclockwise, ccw), select as
# initial point the most Left/Lower corner. Then make as many break
# points as changes in the BC types are.

# In our case, we want 'reflective BC from point v0 to point v2
# Then from point v2 to point v4 we want transmissive BC.
# Finally, from point v4 and back to point v0 we want forced BC.
# Accordingly we define two arrays:

BC_points = np.array([v0,v2,v4,v0])
BC_type = np.array([1,0,2], dtype = int)

# Meaning: in [v0,v2] we want reflective BC (=1), in [v2,v4] we want
# transmissive BC (=0) and, finally, in [v4,v0] we want forced BC (=2)

# for forced BC specify the forced conditions:
BC_forced = np.array(Left)  # meaning, [p_Left,d_Left,u_Left,v_Left]
# if there are no forced conditions, specify instead:
# BC_forced = np.array([0,0,0,0])

# -----------------------------------------------------------
#               GRID GRANULARITY
# -----------------------------------------------------------

# h0 defines the grid granularity. Recommended choices of h0 are:
# h0 = 1.0 (with qmin=0.72). Generates 573 triangles 
# h0 = 0.5 (with qmin=0.7). Generates 2,588 triangles
# h0 = 0.25 (with qmin=0.7). Generates 11,132 triangles
# for initial testing use the larger h0 (for faster simulation)
# (it is recommended to run DISTMESH separately to come up with
# these recommended values: it is more efficient and flexible)

#h0 = 1.0; qmin = 0.72
#h0 = 0.5; qmin = 0.7
h0 = 0.25; qmin = 0.7

# ------------------------------------------------------------------
#                    CREATE THE GENERAL MESH
#   
# ------------------------------------------------------------------
# Define the region in (x,y) plane in which we will create the mesh
# (slightly larger than the physical domain, needed by DISTMESH to
# create an initial grid of points in a domain slightly larger than
# the physical domain. Slightly, means, for example, 2*h0 longer
# on each side)
xmin = -1.0 
ymin = -1.0
xmax = 26.0 
ymax = 17.5

# any fixed points on the boundary that the mesh must adhere to?
# Yes. The vertices of the polygon we created
pfix = verts

# distance function (used by distmesh)
fd = Polygon(verts)

# size function (used by distmesh)
fh = lambda p: np.ones(len(p))

# ------------------------------------------------------
#       CREATE THE MESH AND THE BOUNDARY
# ------------------------------------------------------
p,t,bars = distmesh(fd,fh,h0,xmin,ymin,xmax,ymax,pfix,dptol=0.005,Iflag=4,qmin=qmin)
tri,bbars = find_boundary(p,t,fh)
boundary_nodes,boundary_bars = boundary_info(p,bbars)

# -----------------------------------------------------
#           BOUNDARY PROCESSING
# -----------------------------------------------------

general = BOUNDARY_PROCESSING_TRI(p,tri,bbars,boundary_nodes,boundary_bars, \
                    BC_points,BC_type)

tri,p,neighbors,matched_pairs,area,e01,e12,e20,ang01,ang12,ang20, \
               trans_matched_pairs,refle_matched_pairs,force_matched_pairs, \
                  refle_bcos,refle_bsin = general()

# Nomenclature: meaning of the additional variables
# trans_matched_pairs: an array giving for each ghost triangle tagged with
# 'transmissive BC' the triangle abutted to it in the physical domain
# Similar for refle_matched_pairs ('reflective' BC) and force_matched_pairs
# ('forced BC'). For the reflective BC we will need also the normal to the
# boundary. This is given in the arrays refle_bcos and refle_bsin, that
# give the cosine and sine of the angle that the normal makes with the
# positive x-axis

# map the initial pressure, density and speeds into the physical mesh
# based on the Initial Conditions specification:
pp,dd,uu,vv = ic_phys_Left_Right(Left,Right,x0,p,tri)

start_time = clock()

# visualization domain
x1 = v0[0]; x2 = v3[0]
y1 = v0[1]; y2 = v3[1]

solver = HLLC_TRI(CFL,pp,dd,uu,vv, \
                  area,e01,e12,e20,ang01,ang12,ang20, \
                  neighbors,matched_pairs,p,tri, \
                  trans_matched_pairs,refle_matched_pairs,force_matched_pairs, \
                  refle_bcos,refle_bsin, \
                  BC_forced,x1,x2,y1,y2,Iplot=40)

dd,uu,vv,pp,ee = solver(sim_time)

end_time = clock()
print 'CPU time = %g [sec]' % (end_time - start_time)

# ------------------------------------------------------
#               VISUALIZATION
# ------------------------------------------------------
vis = VISUAL_2D(x1,x2,y1,y2,sim_time,p,tri,pp,dd,uu,vv)
vis()
