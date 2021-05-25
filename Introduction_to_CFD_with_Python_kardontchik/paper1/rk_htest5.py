"""
   Usage: run rk_htest5

        Solves test # 5 in Toro's chapter 6, Table 6.2 p225
        Compare also with Toro, chapter 14, Figures 14.10 to 14.18.

        Uses the HLLC solver        
        Reconstructs the conserved variables using 3rd order ENO (k=3).
        Uses Runge-Kutta 3rd order for time     
"""
import numpy as np
from numpy import pi as pi
import matplotlib.pylab as plt

from IC_PHYS import *
from VISUAL_1D import *

from EXACT_RS import *
from ENO_1D import *
from HLLC_FLUX import *
from RK_ENO_HLLC_SOLVER import *

plt.close('all')

# ----------------------------------------------------
#               INITIAL CONDITIONS
# ----------------------------------------------------
test_num = 5
# Left and Right states
dL, uL, pL = 1.0,    -19.59745,    1000.0
dR, uR, pR = 1.0,    -19.59745,       0.01
# initial discontinuity at x0
x0 = 0.8
# tangential component of velocity
vL = vR = 0.0

# simulation time and CFL for stability
sim_time = 0.012   # simulation time
CFL = 0.9 # Courant number

# order of variables as per Liska-Wendroff
Left = [pL,dL,uL,vL]
Right = [pR,dR,uR,vR]

# dx: grid granularity
dx = 0.01
# coordinates of the cells
x_cell = np.array([i*dx for i in range(0,101)])
# cell # i extends from x_cell[i] to x_cell[i+1]
# the 1st and last cell will be used as ghost cells
# to impose transmissive BC

# map the initial pressure, density and speeds into the 1D physical mesh
p_ic,d_ic,u_ic,v_ic = ic_phys_1D(Left,Right,x_cell,x0)

# -------------------------------------------------------
#           Exact RS for 1D Euler 
# -------------------------------------------------------
ex = EXACT_RS(dL,uL,pL,dR,uR,pR)
xx,dd,uu,pp,ee = ex(sim_time)

# plots
# Note the exact solution using the module "EXACT_RS" assumes
# that the initial discontinuity is at x0 = 0. We have to shift the
# solution to the appropriate x0 for each test
xx = xx + x0
# plot the solution in x = [0,1] 
graphs = Graphics01(test_num,sim_time,xx,dd,uu,pp,ee)
x01,d01,u01,p01,e01 = graphs()
# -------------------------------------------------------
#           End of Exact RS for 1D Euler 
# -------------------------------------------------------

# -------------------------------------------------------
#           RK_ENO_HLLC_SOLVER_1D (in RK_ENO_HLLC SOLVER)
# -------------------------------------------------------
interp = 0 # solution with no ENO
wa = RK_ENO_HLLC_SOLVER_1D(CFL,dx,p_ic,d_ic,u_ic,v_ic,interp)
dd,uu,vv,pp,ee = wa(sim_time)

vis = VISUAL(test_num,CFL,x_cell,x01,d01,u01,p01,e01, \
             dd,uu,pp,ee,interp=interp)
vis()

interp = 1 # solution with ENO
wa = RK_ENO_HLLC_SOLVER_1D(CFL,dx,p_ic,d_ic,u_ic,v_ic,interp)
dd,uu,vv,pp,ee = wa(sim_time)

vis = VISUAL(test_num,CFL,x_cell,x01,d01,u01,p01,e01, \
             dd,uu,pp,ee,interp=interp)
vis()
