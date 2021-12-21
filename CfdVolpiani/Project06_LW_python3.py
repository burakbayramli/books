#! /usr/bin/env python
# -*- coding:utf-8 -*-

################################################################
#
# Class 06: 1-D Euler system of equations
# Solve equation using the Lax-Wendroff scheme
# Author: P. S. Volpiani
# Date: 01/08/2020
#
################################################################

import os, sys
import numpy as np
import matplotlib.pyplot as plt

from numpy import *
from matplotlib import rc

rc('font', family='serif')
rc('lines', linewidth=1.5)
rc('font', size=14)
#plt.rc('legend',**{'fontsize':11})

######################################################################
#             Solving 1-D Euler system of equations
#                 using the Lax-Wendroff scheme
#
#             dq_i/dt + df_i/dx = 0, for x \in [a,b]
#
# This code solves the Sod's shock tube problem (IC=1)
#
# t=0                                 t=tEnd
# Density                             Density
#   ****************|                 *********\
#                   |                           \
#                   |                            \
#                   |                             ****|
#                   |                                 |
#                   |                                 ****|
#                   ***************                       ***********
#
# Domain cells (I{i}) reference:
#
#                |           |   u(i)    |           |
#                |  u(i-1)   |___________|           |
#                |___________|           |   u(i+1)  |
#                |           |           |___________|
#             ...|-----0-----|-----0-----|-----0-----|...
#                |    i-1    |     i     |    i+1    |
#                |-         +|-         +|-         +|
#              i-3/2       i-1/2       i+1/2       i+3/2
#
######################################################################


# Parameters
CFL    = 0.50               # Courant Number
gamma  = 1.4                # Ratio of specific heats
ncells = 400                # Number of cells
x_ini =0.; x_fin = 1.       # Limits of computational domain
dx = (x_fin-x_ini)/ncells   # Step size
nx = ncells+1               # Number of points
x = np.linspace(x_ini+dx/2.,x_fin,nx) # Mesh

# Build IC
r0 = zeros(nx)
u0 = zeros(nx)
p0 = zeros(nx)
halfcells = int(ncells/2)

IC = 1 # 6 IC cases are available
if IC == 1:
    print ("Configuration 1, Sod's Problem")
    p0[:halfcells] = 1.0  ; p0[halfcells:] = 0.1;
    u0[:halfcells] = 0.0  ; u0[halfcells:] = 0.0;
    r0[:halfcells] = 1.0  ; r0[halfcells:] = 0.125;
    tEnd = 0.20;
elif IC== 2:
    print ("Configuration 2, Left Expansion and right strong shock")
    p0[:halfcells] = 1000.; p0[halfcells:] = 0.1;
    u0[:halfcells] = 0.0  ; u0[halfcells:] = 0.0;
    r0[:halfcells] = 3.0  ; r0[halfcells:] = 0.2;
    tEnd = 0.01;
elif IC == 3:
    print ("Configuration 3, Right Expansion and left strong shock")
    p0[:halfcells] = 7.   ; p0[halfcells:] = 10.;
    u0[:halfcells] = 0.0  ; u0[halfcells:] = 0.0;
    r0[:halfcells] = 1.0  ; r0[halfcells:] = 1.0;
    tEnd = 0.10;
elif IC == 4:
    print ("Configuration 4, Shocktube problem of G.A. Sod, JCP 27:1, 1978")
    p0[:halfcells] = 1.0  ; p0[halfcells:] = 0.1;
    u0[:halfcells] = 0.75 ; u0[halfcells:] = 0.0;
    r0[:halfcells] = 1.0  ; r0[halfcells:] = 0.125;
    tEnd = 0.17;
elif IC == 5:
    print ("Configuration 5, Lax test case: M. Arora and P.L. Roe: JCP 132:3-11, 1997")
    p0[:halfcells] = 3.528; p0[halfcells:] = 0.571;
    u0[:halfcells] = 0.698; u0[halfcells:] = 0.0;
    r0[:halfcells] = 0.445; r0[halfcells:] = 0.5;
    tEnd = 0.15;
elif IC == 6:
    print ("Configuration 6, Mach = 3 test case: M. Arora and P.L. Roe: JCP 132:3-11, 1997")
    p0[:halfcells] = 10.33; p0[halfcells:] = 1.0;
    u0[:halfcells] = 0.92 ; u0[halfcells:] = 3.55;
    r0[:halfcells] = 3.857; r0[halfcells:] = 1.0;
    tEnd = 0.09;

E0 = p0/((gamma-1.)*r0)+0.5*u0**2 # Total Energy density
a0 = sqrt(gamma*p0/r0)            # Speed of sound
q  = np.array([r0,r0*u0,r0*E0])   # Vector of conserved variables

if (False):
    fig = plt.subplots()
    ax1 = plt.subplot(4, 1, 1)
    #plt.title('Lax-Wendroff scheme')
    plt.plot(x, r0, 'k-')
    plt.ylabel('$rho$',fontsize=18)
    plt.tick_params(axis='x',bottom=False,labelbottom=False)
    plt.grid(True)
    
    ax2 = plt.subplot(4, 1, 2)
    plt.plot(x, u0, 'r-')
    plt.ylabel('$U$',fontsize=18)
    plt.tick_params(axis='x',bottom=False,labelbottom=False)
    plt.grid(True)

    ax3 = plt.subplot(4, 1, 3)
    plt.plot(x, p0, 'b-')
    plt.ylabel('$P$',fontsize=18)
    plt.tick_params(axis='x',bottom=False,labelbottom=False)
    plt.grid(True)
    
    ax4 = plt.subplot(4, 1, 4)
    plt.plot(x, E0, 'g-')
    plt.ylabel('$E$',fontsize=18)
    plt.grid(True)
    plt.xlim(x_ini,x_fin)
    plt.xlabel('x',fontsize=18)
    plt.subplots_adjust(left=0.2)
    plt.subplots_adjust(bottom=0.15)
    plt.subplots_adjust(top=0.95)
    
    plt.show()

# Solver loop
t  = 0
it = 0
a  = a0
dt=CFL*dx/max(abs(u0)+a0)         # Using the system's largest eigenvalue

while t < tEnd:
    
    # I. Predictor step
    # =================
    q0 = q.copy();
    
    # Primary variables
    r=q0[0];
    u=q0[1]/r;
    E=q0[2]/r;
    p=(gamma-1.)*r*(E-0.5*u**2);
    
    # Flux vector of conserved properties
    F0 = np.array(r*u)
    F1 = np.array(r*u**2+p)
    F2 = np.array(u*(r*E+p))
    flux=np.array([ F0, F1, F2 ]);
    
    qm  = np.roll(q0, 1);
    qp  = np.roll(q0,-1);
    fm  = np.roll(flux, 1);
    fp  = np.roll(flux,-1);

    qpHalf = (qp+q0)/2. - dt/(2.*dx)*(fp-flux)
    qmHalf = (qm+q0)/2. - dt/(2.*dx)*(-fm+flux)
    
    # II. Corrector step
    # ==================
        
    r=qpHalf[0];
    u=qpHalf[1]/r;
    E=qpHalf[2]/r;
    p=(gamma-1.)*r*(E-0.5*u**2);
    F0 = np.array(r*u);
    F1 = np.array(r*u**2+p)
    F2 = np.array(u*(r*E+p))
    FqpHalf=np.array([ F0, F1, F2 ]);
    
    r=qmHalf[0];
    u=qmHalf[1]/r;
    E=qmHalf[2]/r;
    p=(gamma-1.)*r*(E-0.5*u**2);
    F0 = np.array(r*u);
    F1 = np.array(r*u**2+p)
    F2 = np.array(u*(r*E+p))
    FqmHalf=np.array([ F0, F1, F2 ]);
    
    dF = FqpHalf - FqmHalf
    
    q = q0-dt/dx*dF;
    q[:,0]=q0[:,0]; q[:,-1]=q0[:,-1]; # BCs
   
  
    # Compute primary variables
    rho=q[0];
    u=q[1]/rho;
    E=q[2]/rho;
    p=(gamma-1.)*rho*(E-0.5*u**2);
    a=sqrt(gamma*p/rho);
    if min(p)<0: print ('negative pressure found!')
    
    # Update/correct time step
    dt=CFL*dx/max(abs(u)+a);
    
    # Update time and iteration counter
    t=t+dt; it=it+1;
    
    # Plot solution
    if it%40 == 0:
        print (it)
        fig,axes = plt.subplots(nrows=4, ncols=1)
        plt.subplot(4, 1, 1)
        #plt.title('Lax-Wendroff scheme')
        plt.plot(x, rho, 'k-')
        plt.ylabel('$rho$',fontsize=16)
        plt.tick_params(axis='x',bottom=False,labelbottom=False)
        plt.grid(True)

        plt.subplot(4, 1, 2)
        plt.plot(x, u, 'r-')
        plt.ylabel('$U$',fontsize=16)
        plt.tick_params(axis='x',bottom=False,labelbottom=False)
        plt.grid(True)

        plt.subplot(4, 1, 3)
        plt.plot(x, p, 'b-')
        plt.ylabel('$p$',fontsize=16)
        plt.tick_params(axis='x',bottom=False,labelbottom=False)
        plt.grid(True)
    
        plt.subplot(4, 1, 4)
        plt.plot(x, E, 'g-')
        plt.ylabel('$E$',fontsize=16)
        plt.grid(True)
        plt.xlim(x_ini,x_fin)
        plt.xlabel('x',fontsize=16)
        plt.subplots_adjust(left=0.2)
        plt.subplots_adjust(bottom=0.15)
        plt.subplots_adjust(top=0.95)
        plt.show()
#        fig.savefig("fig_Sod_LW_it"+str(it)+".pdf", dpi=300)


