"""
   Usage: run case3
      Case 3 in Table 4.3, Liska_Wendroff_02
      
      Reconstructs the Conserved variables using 3rd order ENO (k=3).
      Uses the HLLC solver
      Uses 3rd order Runge-Kutta time update
      
      if interp=1: HLLC flux with ENO
      if interp=0: HLLC flux without ENO     
"""
import numpy as np
from numpy import pi as pi
import matplotlib.pylab as plt
from time import clock

from IC_PHYS import *
from VISUAL_2D_XY import *

from ENO_1D import *
from HLLC_FLUX import *
from RK_ENO_HLLC_SOLVER_2D import *

plt.close('all')

# interp = 0  # HLLC without ENO
interp = 1   # HLLC with ENO

# -----------------------------------------------------------
#           INITIAL CONDITIONS IN EACH QUADRANT
#                    Case # 3:(p,d,u,v)           
# -----------------------------------------------------------
upper_left = [0.3, 0.5323, 1.206, 0.0]
lower_left = [0.029, 0.138, 1.206, 1.206]
upper_right = [1.5, 1.5, 0.0, 0.0]
lower_right = [0.3, 0.5323, 0.0, 1.206]

# quadrant partitions at:
x0 = 0.5; y0 = 0.5

# simulation time and CFL number for stability
t = 0.3  # simulation time
CFL = 0.2  # Courant number

# ---------------------------------------------------------
#           GRID GRANULARITY AND DOMAIN SIZE
# ---------------------------------------------------------
# grid granularity (dx,dy) and domain size (x1,x2,y1,y2)
dx = 0.04 # 0.01
dy = 0.04 # 0.01
x1 = 0.0; x2 = 1.0
y1 = 0.0; y2 = 1.0

x_cells = round((x2-x1)/dx)  # number of cells in the x-direction
y_cells = round((y2-y1)/dy)  # number of cells in the y-direction
x = np.linspace(x1,x2,x_cells+1)
y = np.linspace(y1,y2,y_cells + 1)

# ----------------------------------------------------------
#            Initial (p,d,u,v) in the grid
# ----------------------------------------------------------

# map the initial pressure, density and speeds into the 2D physical mesh
p_ic,d_ic,u_ic,v_ic = ic_phys_2D(upper_left,lower_left,upper_right,lower_right,x,y,x0,y0)
    
# -------------------------------------------------------
#           SOLVER
# -------------------------------------------------------
start_time = clock()

wa = ENO_HLLC_SOLVER_2D(CFL,x,y,dx,dy,p_ic,d_ic,u_ic,v_ic,interp,Iplot=40)
dd,uu,vv,pp,ee = wa(t)

end_time = clock()
print 'CPU time = %g [sec]' % (end_time - start_time)
# -------------------------------------------------------
#           VISUALIZATION of SIMULATION RESULTS 
# -------------------------------------------------------

v = VISUAL_2D_XY(x,y,pp,dd,uu,vv)
v()
