"""
   Usage: run blunt_unif_sim
      (uniform grid)
      solve Euler equations using the HLLC solver and triangular mesh
"""
import numpy as np
from numpy import pi as pi
import matplotlib.pylab as plt
from time import clock

from VISUAL_TRI_GENERAL import *

from DISTMESH import *
from Meshing_Tools import *
from BOUNDARY_PROCESSING_TRI import *
from IC_conditions import *
from Impose_BC import *

from HLLC_FLUX import *
from RK_HLLC_SOLVER_TRI_GENERAL import *

from DEF_BLUNT_NOSE import *

plt.close('all')


# -------------------------------------------------------
#           MESH DATA
# -------------------------------------------------------

data = np.load('blunt_mesh_unif_h0_0x1.npz')
h0 = data['h0']
p = data['p']
tri = data['tri']
bbars = data['bbars']
boundary_nodes = data['boundary_nodes']
boundary_bars = data['boundary_bars']

# -----------------------------------------------------------
#           INITIAL CONDITIONS: Anderson
#               tuples= (p,d,u,v)
# -----------------------------------------------------------
p_inf = 1.01325e5     # std cond: 1 atm, free-stream pressure
d_inf = 1.225         # std cond [kg/m^3], free-stream density
gamma = 1.4           # calorically perfect gas
Mach_number = 4
a = np.sqrt(gamma*p_inf/d_inf)  # free-stream speed of sound

# Initial Data:
Mach_number = 4
p_Left = p_inf
d_Left = d_inf
u_Left = Mach_number*a
v_Left = 0

p_Right = p_inf
d_Right = d_inf
u_Right = 0
v_Right = 0

Left = [p_Left,d_Left,u_Left,v_Left]
Right = [p_Right,d_Right,u_Right,v_Right]

# initial position of the shock wave
x0 = -1.8

# simulation time and Courant number for stability
sim_time = 0.03   # simulation time
CFL = 0.8  # Courant number
# CPU = 14.4 min for sim_time = 0.03 with CFL=0.8

# ---------------------------------------------------------
#           Fixed points on the boundary
#       (to impose the boundary conditions)
# ---------------------------------------------------------
a = 0.427   # Defines the cubic cylinder

# position (xm,ym) of the highest point in the cylinder
xm = 2.0
ym = ((xm + 1.0)/a)**(1.0/3.0)

v0 = [-2,0]
v1 = [-1,0]
v2 = [xm,ym]
v3 = [2,2.4]
v4 = [-2,2.4]

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

# ------------------------------------------------------------------
#                    IMPORT THE MESH (done)
# ------------------------------------------------------------------

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
                  BC_forced,x1,x2,y1,y2,Iplot=200)

dd,uu,vv,pp,ee = solver(sim_time)

end_time = clock()
print 'CPU time = %g [sec]' % (end_time - start_time)

# ------------------------------------------------------
#               VISUALIZATION
# ------------------------------------------------------
vis = VISUAL_2D(x1,x2,y1,y2,sim_time,p,tri,pp,dd,uu,vv)
vis()

# ------------------------------------------------------------------
#           VISUALIZATION (FOR SPEED and SURFACE PRESSURE)
# ------------------------------------------------------------------
# we have to define the z (like density) not on the triangles,
# but on the nodes of the triangles, so len(z) = len(p) and
# not len(z) = len(tri)
# so for each node (each p) find the triangles that share this node
# take the mean value of z over these triangles and assign this
# mean value to (the same index as) p

vvv = np.zeros(len(p))
ppp = np.zeros(len(p))
# speed
sound = np.sqrt(gamma*pp/dd)
veloc = np.sqrt(uu**2 + vv**2)/sound  # normalized to local Mach speed

for i in range(len(p)):
    ind0 = np.where(tri[:,0] == i)
    ind1 = np.where(tri[:,1] == i)
    ind2 = np.where(tri[:,2] == i)
    vvv[i] = np.mean(np.concatenate((veloc[ind0],veloc[ind1],veloc[ind2])))
    ppp[i] = np.mean(np.concatenate((pp[ind0],pp[ind1],pp[ind2])))

plt.figure()
plt.gca().set_aspect('equal')
plt.tricontourf(p[:,0],p[:,1],tri,vvv,30)
plt.colorbar()
plt.title('SPEED [IN MACH UNITS]')
plt.xlabel('x')
plt.ylabel('y')
plt.show()

# subsonic region
#v_reg = np.zeros(len(vvv))
v_reg = vvv
inda = np.where(vvv <= 1)
indb = np.where(vvv > 1)
#v_reg[inda] = -1
v_reg[indb] = 2

plt.figure()
plt.gca().set_aspect('equal')
plt.tricontourf(p[:,0],p[:,1],tri,v_reg,30)
plt.colorbar()
plt.title('SUBSONIC SPEED REGION')
plt.xlabel('x')
plt.ylabel('y')
plt.show()

# ----------------------------------------------------
#       SURFACE PRESSURE DISTRIBUTION
# ----------------------------------------------------
ind1 = np.where(boundary_nodes == 1)
ind2 = np.where(boundary_nodes == 2)
# note: bind1[0] = 7, meaning boundary_nodes[7] = 1
#       bind2[0] = 31, meaning boundary_nodes[31] = 2
surface_nodes = boundary_nodes[int(ind1[0]):int(ind2[0])+1]
surface_pressure = ppp[surface_nodes[0:]]
# normalized pressure with respect to the stagnation point
p0 = surface_pressure[0]
surface_pressure = surface_pressure/p0
Y_coord = p[surface_nodes,1]

plt.figure()
plt.plot(Y_coord,surface_pressure)
plt.grid()
plt.title('SURFACE PRESSURE AT SIM TIME = %6.4f' % (sim_time))
plt.xlabel('Y')
plt.ylabel('P/P0')
p_1atm = 1.01325e5
if p0 > 1.0e5:
    # change from Pascal units to 1 atm units
    p0 = p0/p_1atm
    plt.text(0.1,0.15,'PRESSURE AT STAGNATION POINT, P0 = %g ATM' %(p0))
else:
    plt.text(0.1,0.15,'PRESSURE AT STAGNATION POINT, P0 = %g PASCAL' %(p0))
plt.show()
