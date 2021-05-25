"""
   Usage: run rk_airfoil_M0x8_1x25deg_sim
      note: uses initial uniform IC over all the physical domain

   Uses Runge-Kutta
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

plt.close('all')


# -------------------------------------------------------
#           MESH DATA
# -------------------------------------------------------
data = np.load('flat_mesh1_1x25deg.npz')
h0 = data['h0']
p = data['p']
tri = data['tri']
bbars = data['bbars']
ext_bnodes = data['ext_bnodes']
ext_bbars = data['ext_bbars']
int_bnodes = data['int_bnodes']
int_bbars = data['int_bbars']

# -----------------------------------------------------------
#           INITIAL CONDITIONS
#               tuples= (p,d,u,v)
# -----------------------------------------------------------
p_inf = 1.01325e5     # std cond: 1 atm, free-stream pressure
d_inf = 1.225         # std cond [kg/m^3], free-stream density
gamma = 1.4           # calorically perfect gas

a = np.sqrt(gamma*p_inf/d_inf)  # free-stream speed of sound

# Initial Data:
Mach_number = 0.8

p_Left = p_inf
d_Left = d_inf
u_Left = Mach_number*a
v_Left = 0

p_Right = p_Left
d_Right = d_Left
u_Right = u_Left
v_Right = v_Left

Left = [p_Left,d_Left,u_Left,v_Left]
Right = [p_Right,d_Right,u_Right,v_Right]

# initial position of the shock wave
x0 = -1.0 

# simulation time and Courant number for stability
sim_time = 0.032 
CFL = 0.8 # Courant number

# CPU = 1.1 min for sim_time = 0.0002 (32 iter using CFL=0.8)
# CPU = 3.3 hours for sim_time = 0.032 (5754 iter using CFL=0.8, Iplot=500)

# ---------------------------------------------------------
#           Fixed points on the boundary
#       (to impose the boundary conditions)
# ---------------------------------------------------------
# add node '-1' to separate external boundary nodes from internal
# boundary nodes
sep_bnodes = np.array([-1])
boundary_nodes = np.concatenate((ext_bnodes,sep_bnodes,int_bnodes))
boundary_bars = np.concatenate((ext_bbars,int_bbars))

# ext_bnodes is traversed ccw leaving the computational domain to the left
# int_bnodes is traversed cw, leaving the computational domain to the left
# i.e., if we 'sit' on the arrow defined as going from p[ext_bnodes[i]] to
# p[ext_bnodes[i+1]) we will see the computational domain to our left.
# similarly, if we 'sit' on the arrow defined as going from p[int_bnodes[i]]
# to p[int_bnodes[i+1]], we will see the comp domain again to our left

# we want forced BC on the Left side of the external boundary and
# transmissive BC on the other three sides of the external boundary

# Define the extreme nodes on the external boundary for every region of BC:
# The four node corners of the wind tunnel are p[N:N+4], where N is the
# number of points on the airfoil (taken as 58, for NACA00012, we eliminated
# two points too close to the tail), hence the wind tunnel node corners
# are p[58:62]
v1 = p[58]  # lower left corner node of external boundary
v2 = p[61] # upper left corner node of external boundary
# from v1 to v2 we want transmissive BC
# from v2 to v1 we want forced BC

# we want reflective BC on all the internal boundary (the airfoil)

# Define the extreme nodes on the internal boundary for every region of BC:
# Since we impose reflective BC on all the airfoil, it is sufficient to choose
# any two nodes on the internal boundary
# Looking at the coordinates of the int_bnodes (for ex, by typing in IPython
# p[int_bnodes] and looking for the nodes corresponding to the head and
# tail of the airfoil), we see:

v3 = p[int_bnodes[0]]  # near the head of airfoil
v4 = p[int_bnodes[30]]  # near the tail of airfoil
# from v3 to v4 we want reflective BC
# from v4 to v3 we want reflective BC
# that is, we want reflective BC on all the airfoil


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

# In our case, we want:
# On the external boundary:
# 'transmissive BC from point v1 to point v2
# Then from point v2 to point v1 we want forced BC.
# On the internal boundary (the airfoil), we want
# reflective BC everywhere

# sep_v separates the external boundary nodes from the internal boundary nodes
sep_v = np.array([-999.9,-999.9])
# sep_t separates the external boundary types from the internal boundary types
sep_t = -1

BC_points = np.array([v1,v2,v1,sep_v,v3,v4,v3])
BC_type = np.array([0,2,sep_t,1,1], dtype = int)

# Meaning: for the external boundary:
# in [v1,v2] we want transmissive BC (=0), in [v2,v1] we want forced BC (=2)
# and for the internal boundary (the airfoil):
# we want in [v3,v4] (from head to tail on the upper surface) we want
# reflective BC ( = 1) and in [v4,v3] (from tail to head on the lower surface)
# we want also reflective BC ( = 1)

# for forced BC specify the forced conditions:
BC_forced = np.array(Left)  # meaning, [p_Left,d_Left,u_Left,v_Left]
# if there are no forced conditions, specify instead:
# BC_forced = np.array([0,0,0,0])

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
x1 = -2.0; x2 = 3.0
y1 = -1.5; y2 = 1.5

solver = HLLC_TRI(CFL,pp,dd,uu,vv, \
                  area,e01,e12,e20,ang01,ang12,ang20, \
                  neighbors,matched_pairs,p,tri, \
                  trans_matched_pairs,refle_matched_pairs,force_matched_pairs, \
                  refle_bcos,refle_bsin, \
                  BC_forced,x1,x2,y1,y2,Iplot=500)

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
ddd = np.zeros(len(p))
# speed
sound = np.sqrt(gamma*pp/dd)
veloc = np.sqrt(uu**2 + vv**2)/sound  # normalized to local Mach speed

for i in range(len(p)):
    ind0 = np.where(tri[:,0] == i)
    ind1 = np.where(tri[:,1] == i)
    ind2 = np.where(tri[:,2] == i)
    vvv[i] = np.mean(np.concatenate((veloc[ind0],veloc[ind1],veloc[ind2])))
    ppp[i] = np.mean(np.concatenate((pp[ind0],pp[ind1],pp[ind2])))
    ddd[i] = np.mean(np.concatenate((dd[ind0],dd[ind1],dd[ind2])))

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
# find the tail. By construction of the flatten mesh:
ind = np.min(int_bnodes)  # assumes a pointed tail, as in NACA0012
# Then, nodes and surface pressure from tail to head along the upper surface,
# followed by nodes and pressure from head to tail along the lower surface:
p_airfoil = p[ind:ind + len(int_bnodes)]
surface_pressure = ppp[ind: ind + len(int_bnodes)]
pmax = np.max(surface_pressure)

p_airfoil_upper = p[ind:ind + len(int_bnodes)/2 + 1]
p_airfoil_lower = p[ind + len(int_bnodes)/2 + 1:ind + len(int_bnodes)]
surf_pressure_upper = surface_pressure[0:len(int_bnodes)/2 + 1]
surf_pressure_lower = surface_pressure[len(int_bnodes)/2+1:]

plt.figure()
plt.plot(p_airfoil_upper[:,0],surf_pressure_upper/pmax,'b--', \
         p_airfoil_lower[:,0],surf_pressure_lower/pmax,'r-')
plt.grid()
plt.title('SURFACE PRESSURE AT SIM TIME = %g' % (sim_time))
plt.xlabel('AIRFOIL X-COORDINATE')
p_1atm = 1.01325e5
p0 = pmax/p_1atm
plt.ylabel('P/PMAX,     PMAX = %g ATM' % (p0))
plt.legend(('upper','lower'))
plt.show()

# ----------------------------------------------------
#       SURFACE PRESSURE COEFFICIENT
#       Cp = (p - p[infinity])/q[infinity]
# ----------------------------------------------------
p_inf = p_Left
d_inf = d_Left
v_inf = u_Left
q_inf = 0.5*d_inf*v_inf**2

# surface pressure coefficient
upper_p_coeff = (surf_pressure_upper - p_inf)/q_inf
lower_p_coeff = (surf_pressure_lower - p_inf)/q_inf

# Note: it is customary to plot -Cp instead of +Cp
plt.figure()
plt.plot(p_airfoil_upper[:,0],-upper_p_coeff,'b-', \
         p_airfoil_lower[:,0],-lower_p_coeff,'r--')
plt.grid()
plt.title('SURFACE PRESSURE COEFF AT SIM TIME = %6.4f' % (sim_time))
plt.xlabel('AIRFOIL X COORDINATE')
plt.ylabel('SURFACE PRESSURE COEFF, - Cp')
plt.legend(('upper','lower'), loc = 'lower right')
plt.show()

