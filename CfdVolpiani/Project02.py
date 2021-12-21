#! /usr/bin/env python
# -*- coding:utf-8 -*-

################################################################
#
# Class 02: 1D-Diffusion problem
# Objective: Solve equation using the Forward-Time Central-Space scheme
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
plt.rc('legend',**{'fontsize':14})

#===============================================================
# Define parameters
#===============================================================

alpha = 0.01                      # Diffusivity
sigma = 0.4                       # Stability condition
Nx = 101;                         # Number of grid points
xmax = 2.;                        # Domain limit to the right
xmin = -2.;                       # Domain limit to the left
dx = (xmax-xmin)/(Nx-1)           # Mesh size
x = np.linspace(xmin,xmax,Nx)     # Discretized mesh
dt = sigma*dx**2/alpha            # Time step
t_end = 5.                        # Final time
Nt = int(t_end/dt)                # Number of iterations
t = np.linspace(0.,t_end,Nt+1)    # Time vector
U = np.zeros((Nt+1,Nx))           # u^n_i
U[0,:] = np.exp(-0.5*(x/0.4)**2)  # Initial solution


#===============================================================
# Temporal loop
#===============================================================

for n in range (0,Nt):
  
  sigma = alpha*dt/(dx*dx)
  
  for i in range (1,Nx-1):
      U[n+1,i] = U[n,i] + sigma*(U[n,i+1]-2*U[n,i]+U[n,i-1]); # Interior points
  U[n+1,0] = 0;                                               # BC left
  U[n+1,-1] = 0;                                              # BC right
  
  # Plot solution
  if (n==0): fig, ax = plt.subplots(figsize=(5.5,4))
  plt.clf()
  plt.plot(x,U[n+1,:])
  plt.scatter(x,U[0,:], marker='o', facecolors='white', color='k')
  plt.gca().legend(('Numerical result ($\sigma$='+str(sigma)+')','Initial condition'))
  plt.axis([xmin, xmax, 0, 1.4])
  plt.title('t='+str(round(dt*(n+1),3)),fontsize=16)
  plt.xlabel('x',fontsize=18)
  plt.ylabel('u',fontsize=18)
  plt.subplots_adjust(left=0.2)
  plt.subplots_adjust(bottom=0.18)
  plt.draw()
  plt.pause(0.001)

plt.show()
#fig.savefig("figure.pdf", dpi=300)
