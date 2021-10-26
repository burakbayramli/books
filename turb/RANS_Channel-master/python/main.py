#**************************************************************************
#
# RANS solver for fully developed turbulent channel with varying properties
# 
#       Created on: Nov 22, 2017
#          Authors: Rene Pecnik         (R.Pecnik@tudelft.nl)
#                   Gustavo J. Otero R. (G.J.OteroRodriguez@tudelft.nl)
#                   Process & Energy Department, Faculty of 3mE
#                   Delft University of Technology, the Netherlands.
#       Literature: Otero et al., 2018. Heat and fluid flow
# Last modified on: Jan 04, 2019
#               By: Rene Pecnik 
#**************************************************************************


import matplotlib
#matplotlib.use("TkAgg")            ####### added for MAC
import matplotlib.pyplot as plt
matplotlib.rcParams.update({'font.size': 14})
matplotlib.rcParams['figure.figsize'] = [16,10]

import numpy as np

from mesh import Mesh
from solveEqn import solveEqn
from solveRANS import solveRANS

from velTransSLS import velTransSLS
from velTransVD import velTransVD

import linecache

## ------------------------------------------------------------------------
#
# User defined inputs
#

# -----  choose test case   -----
# constProperty  ... constant properties: rho = 1; mu = 1/ReT
# constReTauStar ... constant semi-local Reynolds number, rho and mu variable
# gasLike        ... gas-like fluid behaviour
# liquidLike     ... liquid-like fluid behaviour
casename = 'constReTauStar'

# -----  choose turbulence model  -----
# 'Cess'... Cess, R.D., "A survery of the literature on heat transfer in 
#           turbulent tube flow", Tech. Rep. 8-0529-R24, Westinghouse, 1958.
# 'SA'  ... Spalart, A. and Allmaras, S., "One equation turbulence model for 
#           aerodynamic flows", Recherche Aerospatiale-French edition, 1994.
# 'MK'  ... Myong, H.K. and Kasagi, N., "A new approach to the improvement of
#           k-epsilon turbulence models for wall bounded shear flow", JSME 
#           Internationla Journal, 1990.
# 'SST' ... Menter, F.R., "Zonal Two equation k-omega turbulence models for 
#           aerodynamic flows", AIAA 93-2906, 1993.
# 'V2F' ... Medic, G. and Durbin, P.A., "Towards improved prediction of heat 
#           transfer on turbine blades", ASME, J. Turbomach. 2012.
# 'no'  ... without turbulence model; laminar
# turbModel

# -----  compressible modification  -----
# 0 ... Conventional models without compressible modifications
# 1 ... Otero et al.
# compressCorrection

# ----mesh-  solve energy equation  ----- 
# 0 ... energy eq not solved, density and viscosity taken from DNS
# 1 ... energy eq solved
# solveTemperatureEq


## ------------------------------------------------------------------------
#
# Generate mesh
#
height = 2      # channel height
n = 100          # number of mesh points
fact = 6        # streching factor and stencil for finite difference discretization
mesh = Mesh(n, height, fact, 1) 

file = "../DNS_data/"+ casename + ".txt" # chose case

print("name of the file ", file)

# get parameters from DNS or set these parameters (they must be defined)
line = linecache.getline(file, 39)[1:].split()       
ReTau  = float(line[0]); print("ReTau  = ", ReTau)   # Reynolds number
Pr     = float(line[1]); print("Pr     = ", Pr)      # Prandtl number
expRho = float(line[2]); print("expRho = ", expRho)  # temperature exponent for density 
expMu  = float(line[3]); print("expMu  = ", expMu)   # temperature exponent for viscosity 
expLam = float(line[4]); print("expLam = ", expLam)  # temperature exponent for conductivity 
Qvol   = float(line[5]); print("Qvol   = ", Qvol)    # volmetric heating

# load dns data
DNS = np.loadtxt(file,skiprows=88)

# interpolate density from DNS on RANS mesh
r = np.interp(np.minimum(mesh.y, mesh.y[-1]-mesh.y) , DNS[:,0], DNS[:,5])
r[0] = r[-1] = 1.0

# interpolate viscosity from DNS on RANS mesh
mu = np.interp(np.minimum(mesh.y, mesh.y[-1]-mesh.y) , DNS[:,0], DNS[:,6])
mu[0] = mu[-1] = 1.0/ReTau

## ------------------------------------------------------------------------
# solve RANS 1 (model 1)
turbModel          = "V2F"    # turbulence model
compressCorrection = 0       # corrections for varying properties
solveTemperatureEq = 1       # if not temp equation solved then use rho and mu from DNS 
u1,T1,r1,mu1,mut1,k1,e1,om1 = solveRANS(r,mu,mesh,turbModel,compressCorrection,solveTemperatureEq,Pr,ReTau,expLam,expRho,expMu,Qvol)

# solve RANS 2 (model 2)
turbModel          = "V2F"    # turbulence model
compressCorrection = 1       # corrections for varying properties
solveTemperatureEq = 1       # if not temp equation solved then use rho and mu from DNS 
u2,T2,r2,mu2,mut2,k2,e2,om2 = solveRANS(r,mu,mesh,turbModel,compressCorrection,solveTemperatureEq,Pr,ReTau,expLam,expRho,expMu,Qvol)

    
## ------------------------------------------------------------------------
# plot results
#    
n = mesh.nPoints

# model 1
ypl1,uvd1 = velTransVD(u1,r1,ReTau,mesh)      # note, u is already u+ due to normalization
yst1,ust1 = velTransSLS(u1,r1,mu1,ReTau,mesh)

# model 2
ypl2,uvd2 = velTransVD(u2,r2,ReTau,mesh)      # note, u is already u+ due to normalization
yst2,ust2 = velTransSLS(u2,r2,mu2,ReTau,mesh)

ax = plt.subplot(2,3, 1)
# analytic results for viscous sub-layer
ypLam = np.linspace(0.2,13,100); 
ax.semilogx(ypLam,ypLam,'k-.')
    
# semi-empirical result for log-layer
ypTurb = np.linspace(0.9,3,20); 
upTurb = 1/0.41*np.log(np.power(10, ypTurb))+5.2
ax.semilogx(np.power(10, ypTurb), upTurb,'k-.')

#########
# model solution
line1 = ax.semilogx(ypl1[1:n//2],u1[1:n//2], 'r-',linewidth=3)
line2 = ax.semilogx(ypl2[1:n//2],u2[1:n//2], 'b--',linewidth=3)
line3 = ax.plot(DNS[::3,1],DNS[::3,8],'o',fillstyle='none',linewidth=4, markersize=8)
plt.xlabel('$y^+$', fontsize=16); plt.ylabel('$u^+$', fontsize=16);
plt.legend(('$y^+$','log','model 1','model 2', 'DNS'))

plt.subplot(2,3, 2)
plt.semilogx(ypLam,ypLam,'k-.'); plt.semilogx(np.power(10, ypTurb), upTurb,'k-.')
plt.semilogx(ypl1[1:n//2],uvd1[1:n//2],'r-',linewidth=3)
plt.semilogx(ypl2[1:n//2],uvd2[1:n//2],'b--',linewidth=3)
plt.plot(DNS[::3,1],DNS[::3,10],'o',fillstyle='none',linewidth=4, markersize=8)
plt.xlabel('$y^+$', fontsize=16); plt.ylabel('$u^{vD}$', fontsize=16);

plt.subplot(2,3, 3)
plt.semilogx(ypLam,ypLam,'k-.'); plt.semilogx(np.power(10, ypTurb), upTurb,'k-.')
plt.semilogx(yst1[1:n//2],ust1[1:n//2],'r-',linewidth=3)
plt.semilogx(yst2[1:n//2],ust2[1:n//2],'b--',linewidth=3)
plt.plot(DNS[::3,2],DNS[::3,11],'o',fillstyle='none',linewidth=4, markersize=8)
plt.xlabel('$y^\star$', fontsize=16); plt.ylabel('$u^\star$', fontsize=16);

plt.subplot(2,3, 4)
plt.plot(mesh.y[0:n//2],r1[0:n//2],'r-',linewidth=3)
plt.plot(mesh.y[0:n//2],r2[0:n//2],'b--',linewidth=3)
plt.plot(DNS[::5,0],DNS[::5,5],'o',fillstyle='none',linewidth=4, markersize=8)
plt.xlabel('$y$', fontsize=16); plt.ylabel('$rho$', fontsize=16);

plt.subplot(2,3, 5)
plt.plot(mesh.y[0:n//2],mu1[0:n//2]/mu[0],'r-',linewidth=3)
plt.plot(mesh.y[0:n//2],mu2[0:n//2]/mu2[0],'b--',linewidth=3)
plt.plot(DNS[::5,0],DNS[::5,6]/mu[0],'o',fillstyle='none',linewidth=4, markersize=8)
plt.xlabel('$y$', fontsize=16); plt.ylabel('$\mu$', fontsize=16);

plt.subplot(2,3, 6)
plt.plot(mesh.y[0:n//2],T1[0:n//2],'r-',linewidth=3)
plt.plot(mesh.y[0:n//2],T2[0:n//2],'b--',linewidth=3)
plt.plot(DNS[::5,0],DNS[::5,14],'o',fillstyle='none',linewidth=4, markersize=8)
plt.xlabel('$y$', fontsize=16); plt.ylabel('$Temperature$', fontsize=16);

plt.show()
