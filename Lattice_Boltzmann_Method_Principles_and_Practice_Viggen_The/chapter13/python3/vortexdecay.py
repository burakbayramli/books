# This code accompanies
#   The Lattice Boltzmann Method: Principles and Practice
#   T. Krüger, H. Kusumaatmaja, A. Kuzmin, O. Shardt, G. Silva, E.M. Viggen
#   ISBN 978-3-319-44649-3 (Electronic) 
#        978-3-319-44647-9 (Print)
#   http://www.springer.com/978-3-319-44647-9
#
# This code is provided under the MIT license. See LICENSE.txt.
#
# Author: Erlend M. Viggen
# 
# Example python code for computing a Taylor Green vortex decay using LBM
#
from __future__ import division
from __future__ import print_function

import matplotlib.pyplot as plt
import numpy as np
import time

# simulation parameters
scale  = 2               # set simulation size
NX     = 32*scale        # domain size
NY     = NX
NSTEPS = 200*scale*scale # number of simulation time steps
NMSG   = 50*scale*scale  # show messages every NMSG time steps
vis    = False           # show visualisation; set to False for performance measurements
NVIS   = NMSG            # show visualisation every NVIS time steps
tau    = 1               # relaxation time
u_max  = 0.04/scale      # maximum velocity
nu     = (2*tau-1)/6     # kinematic shear viscosity
rho0   = 1               # rest density
Re     = NX*u_max/nu     # Reynolds number; not used in the simulation itself

# Lattice parameters
w = np.array([4/9, 1/9, 1/9, 1/9, 1/9, 1/36, 1/36, 1/36, 1/36]) # weights
c = np.array([[0, 1, 0, -1,  0, 1, -1, -1,  1],  # velocities, x components
              [0, 0, 1,  0, -1, 1,  1, -1, -1]]) # velocities, y components

x = np.arange(NX)+0.5
y = np.arange(NY)+0.5
[X, Y] = np.meshgrid(x,y)

# Function to calculate the equilibrium distribution
def equilibrium(rho, u):
    cdot3u = 3 * np.einsum('ai,axy->xyi', c, u)
    usq = np.einsum('axy->xy', u*u)
    feq = np.einsum('i,xy->xyi', w, rho) * (1 + cdot3u*(1 + 0.5*cdot3u) - 1.5*usq[:,:,np.newaxis])
    return feq

# Function to calculate the Taylor-Green vortex solution
def taylorgreen(t, nu, rho0, u_max):
    kx = 2*np.pi/NX
    ky = 2*np.pi/NY
    td = 1/(nu*(kx*kx+ky*ky))
    
    u = np.array([-u_max*np.sqrt(ky/kx)*np.cos(kx*X)*np.sin(ky*Y)*np.exp(-t/td),
                   u_max*np.sqrt(kx/ky)*np.sin(kx*X)*np.cos(ky*Y)*np.exp(-t/td)])
    P = -0.25*rho0*u_max*u_max*((ky/kx)*np.cos(2*kx*X)+(kx/ky)*np.cos(2*ky*Y))*np.exp(-2*t/td)
    rho = rho0+3*P
    return [rho, u, P]

# Initialise populations
[rho, u, P] = taylorgreen(0, nu, rho0, u_max)
f = equilibrium(rho, u)

print('Simulating Taylor-Green vortex decay')
print('      domain size:', str(NX) + 'x' + str(NY))
print('               nu:', nu)
print('              tau:', tau)
print('            u_max:', u_max)
print('             rho0:', rho0)
print('        timesteps:', NSTEPS)
print('       plot every:', NVIS)
print('    message every:', NMSG)
print('')

E = np.einsum('xy,axy->', rho, u*u)
print(0, E, 0, 0, 0)

tstart = time.time()

# Main loop
for t in range(1, NSTEPS+1):
    
    # Periodic streaming using numpy's roll operation
    for i in range(9):
        f[:,:,i] = np.roll( np.roll(f[:,:,i], c[0,i], axis=1), c[1,i], axis=0 )
    
    # Compute macroscopic quantities
    rho = np.einsum('xyi->xy', f)
    u   = np.einsum('ai,xyi->axy', c, f) / rho
    
    # Collision step
    f = (1-1/tau)*f + (1/tau)*equilibrium(rho, u)
    
    # Compare against the analytical solution every NMSG time steps
    if t % NMSG == 0:
        # Calculate analytical solution
        [rhoa, ua, Pa] = taylorgreen(t, nu, rho0, u_max)
        
        # Kinetic energy
        E = np.einsum('xy,axy->', rho, u*u)
        
        # Sum square errors
        sumrhoe2 = np.einsum('xy->', (rho-rhoa)*(rho-rhoa))
        sumue2   = np.einsum('axy->a', (u-ua)*(u-ua))
        
        # Normalisation factors
        sumrhoa2 = np.einsum('xy->', (rhoa-rho0)*(rhoa-rho0))
        sumua2   = np.einsum('axy->a', ua*ua)
        
        # L2 norms
        L2rho = np.sqrt( sumrhoe2  / sumrhoa2 )
        L2ux  = np.sqrt( sumue2[0] / sumua2[0] )
        L2uy  = np.sqrt( sumue2[1] / sumua2[1] )
        
        print(t, E, L2rho, L2ux, L2uy)
    
    # Plot solution every NVIS time steps
    if vis and t % NVIS == 0:
        umag = np.sqrt( np.einsum('axy->xy', u*u) ) / u_max
        plt.imshow(umag, extent=[0,1,0,1], vmin=0, vmax=1)
        bar = plt.colorbar()
        plt.streamplot(x/NX, y/NY, u[0,:,:], u[1,:,:], color=[1,1,1])
        plt.xlabel('$x/l_x$')
        plt.ylabel('$y/l_y$')
        bar.set_label('$|\mathbf{u}|/u_\mathrm{max}$')
        td = 1/(nu*(2*np.pi/NX)**2 + (2*np.pi/NY)**2)
        plt.title('flow field at $t/t_d = {0:.2f}$'.format(t/td))
        plt.show()


# Calculate performance information after the simulation is finished
runtime = time.time() - tstart
nodes_updated = NSTEPS*NX*NY
speed = nodes_updated/(1e6*runtime)

print(' ----- performance information -----')
print('        timesteps:', NSTEPS);
print('          runtime:', runtime, '(s)')
print('            speed:', speed, '(Mlups)')

