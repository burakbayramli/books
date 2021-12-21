#! /usr/bin/env python
# -*- coding:utf-8 -*-

################################################################
#
# Class 03: 1D-Advection problem
# Objective: Solve equation using the Lax-Wendroff scheme
# Author: P. S. Volpiani
# Date: 01/08/2020
#
################################################################

#===============================================================
# Some libraries
#===============================================================

import matplotlib.pyplot as plt
import numpy as np

from matplotlib import rc
rc('font', family='serif')
rc('lines', linewidth=1.5)
rc('font', size=16)
plt.rc('legend',**{'fontsize':12})

#===============================================================
# Some definitions
#===============================================================

scheme = 2                        # Chose your scheme: 1 (upwind), 2 (centered/Lax-Wendroff)
Nx = 101;                         # Number of grid points
xmax = 2.;                        # Domain limit to the right
xmin = -2.;                       # Domain limit to the left
dx = (xmax-xmin)/(Nx-1)           # Mesh size
dt = 0.04                         # Time step
c = 0.8                           # Advection speed
CFL = c*dt/dx                     # CFL number
x = np.arange(xmin,xmax,dx)       # Discretized mesh
U = np.exp( -0.5 * (x/0.4)**2 )   # Initial solution
Uex = U                           # Exact solution
t_end = 5.                        # Final time
Nt = int(t_end/dt)                # Number of iterations
t = np.linspace(0.,t_end,Nt+1)    # Time vector


#===============================================================
# Temporal loop
#===============================================================
for n in range (1,len(t)):
  
  # Solve equation using upwind scheme
  if (scheme == 1):
    
      Un = U
      if (c>0.):
          Um = np.roll(Un,1)
          U = Un - CFL*(Un-Um)
      else:
          Up = np.roll(Un,-1)
          U = Un - CFL*(Up-Un)
          
  # Solve equation using the centered scheme with/without dissipation
  if (scheme == 2):
  
      theta = (c*dt/dx)**2;
      Un = U
      Um = np.roll(Un,1)
      Up = np.roll(Un,-1)
      U  = Un - 0.5*CFL*(Up-Um) + 0.5*theta*(Up-2*Un+Um)
      
  #===============================================================
  # Compute exact solution
  #===============================================================
  d = c*n*dt
  Uex = np.exp(-0.5*(np.mod(x-d+xmax,4)-xmax)**2/0.4**2)
  errL1 = U - Uex
  errL2 = np.linalg.norm(errL1)
  
  #===============================================================
  # Plot solution
  #===============================================================
  if (n==1): fig, ax = plt.subplots(figsize=(5.5,4))
  plt.clf()
  plt.plot(x,U)
  plt.scatter(x,Uex, marker='o', facecolors='white', color='k')
  plt.gca().legend(('Centered scheme ($\\theta$='+str(round(theta,3))+', CFL='+str(CFL)+')','Exact solution'))
  plt.axis([xmin, xmax, 0, 1.4])
  plt.title('t='+str(round(dt*n,3)),fontsize=16)
  plt.xlabel('x',fontsize=18)
  plt.ylabel('u',fontsize=18)
  plt.subplots_adjust(left=0.2)
  plt.subplots_adjust(bottom=0.18)
  plt.draw()
  plt.pause(0.001)

plt.show()
#fig.savefig("figure.pdf", dpi=300)
print ('Error L2 = ',errL2)

