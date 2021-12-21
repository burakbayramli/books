#! /usr/bin/env python
# -*- coding:utf-8 -*-

################################################################
#
# Class 04: 1D-Inviscid Burgers' equation
# Solve Burgers' equation numerically and plot the characteristic curves
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
plt.rc('legend',**{'fontsize':11})

#===============================================================
# Some definitions
#===============================================================

Nx = 201;                         # Number of grid points
xmax = 4.;                        # Domain limit to the right
xmin = -2.;                       # Domain limit to the left
dx = (xmax-xmin)/(Nx-1)           # Mesh size
x = np.arange(xmin,xmax,dx)       # Discretized mesh

t = 0.                            # Initial time
t_end = 2.                        # Final time
dt = 0.01                         # Time step
Nt = int(t_end/dt)                # Number of iterations
tt = np.linspace(0.,t_end,Nt+1)   # Time vector

# Choose initial solution !
case=4
if (case==1):
  U = np.exp( -0.5 * (x/0.4)**2 )
elif (case==2):
  U = (x<-1.)*(0)+(x<=0)*(x>-1.)*(1+x)+(x<1.)*(x>0.)*(1-x)+(x>1.)*(0)
elif (case==3):
  U = (x<=0)*(0)+(x<1.)*(x>0.)*(x)+(x>1.)*(1)
elif (case==4):
  U = (x<=0)*(1)+(x<1.)*(x>0.)*(1-x)+(x>=1.)*(0)
elif (case==5):
  U = (x<=0)*(0)+(x<1.)*(x>0.)*(1)+(x>=1.)*(0)
Uex0 = U                          # Initial solution

vec = np.zeros((Nt+1,Nx-1))       # u^n_i
n=0                               # counter


#===============================================================
# Solve equation using a conservative scheme
#===============================================================
while (t <= t_end):
  
  n = n+1
  sigma = dt/dx
  t = t+dt
  
  Un = U
  Um = np.roll(Un,1)
  Up = np.roll(Un,-1)
  Ap = 0.5 * (Up + Un)
  Am = 0.5 * (Un + Um)
  
  U = Un - 0.5 * sigma * (Am + abs(Am)) * (Un-Um) - 0.5 * sigma * (Ap - abs(Ap)) * (Up - Un)
  U[0] = Uex0[0]
  U[-1] = Uex0[-1]
  vec[n,:] = U

  #===============================================================
  # Plot solution
  #===============================================================
  if (n==1): fig, ax = plt.subplots(2)
  plt.clf()
  plt.subplot(2,1,1)
  plt.plot(x,U,'k')
  plt.axis([xmin, xmax, 0, 1.5])
  plt.title('t='+str(round(dt*(n+1),3)),fontsize=16)
  plt.xlabel('x',fontsize=16)
  plt.ylabel('u',fontsize=16)
  plt.subplot(2,1,2)
  #plt.contour(x,tt,vec,8,colors='k')
  plt.contour(x,tt,vec,np.linspace(0.1,1.,10),colors='k')
  #plt.colorbar()
  plt.xlabel('x',fontsize=16)
  plt.ylabel('t',fontsize=16)
  
  plt.subplots_adjust(left=0.2)
  plt.subplots_adjust(bottom=0.15)
  plt.draw()
  plt.pause(0.001)

plt.show()
#fig.savefig("fig.pdf", dpi=300)


