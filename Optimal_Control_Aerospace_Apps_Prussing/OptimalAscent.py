# Optimal Ascent Problem with Python's solve_bvp
#
# Original script for MATLAB's bvp4c by by Jose J. Guzman and George E. Pollock
#
# Adapted for Python by Nickolai Belakovski
#
# This script uses scipy's solve_bvp to solve the problem of finding the
# optimal ascent trajectory for launch from a flat Moon to a 100 nautical
# mile circular orbit.


import numpy as np
from scipy.integrate import solve_bvp
import matplotlib.pyplot as plt



# Define parameters of the problem
h = 185.2e3 # meters, final altitude (100 nmi circular orbit)
Vc = 1.627e3 # m/s, Circular speed at 100 nmi
g_accel = 1.62 # m/sˆ2, gravitational acceleration of Moon
Thrust2Weight = 3 # Thrust to Weight ratio for Ascent Vehicle, in lunar G's
#----------------------------------------------------------------------------
## Boundary Conditions
#----------------------------------------------------------------------------
# Initial conditions
# Launch from zero altitude with zero initial velocity
x0 = 0 # meters, initial x-position
y0 = 0 # meters, initial y-position
Vx0 = 0 # m/s, initial downrange velocity
Vy0 = 0 # m/s, initial vertical velocity
# Final conditions
yf = h # meters, final altitude
Vxf = Vc # m/s, final downrange velocity
Vyf = 0 # m/s, final vertical velocity

# Note: this function uses g_accel and Thrust2Weight and so must be defined
# after those two variables
def ascent_odes_tf(tau, X, tf):
    #
    # State and Costate Differential Equation Function for the Flat-Moon
    # Optimal Ascent Problem
    #
    #
    # The independent variable here is the nondimensional time, tau, the state
    # vector is X, and the final time, tf, is an unknown parameter that must
    # also be passed to the DE function.
    # Note that the state vector X has components
    # X[0] = x, horizontal component of position
    # X[1] = y, vertical component of position
    # X[2] = Vx, horizontal component of velocity
    # X[3] = Vy, vertical component of velocity
    # X[4] = lambda2_bar
    # X[5] = lambda4_bar
    # Acceleration (F/m) of the Ascent Vehicle, m/s^2
    Acc = Thrust2Weight*g_accel
    # State and Costate differential equations in terms of d/dt:
    xdot = X[2]
    ydot = X[3]
    Vxdot = Acc*(1/np.sqrt(1+X[5]**2))
    Vydot = Acc*(X[5]/np.sqrt(1+X[5]**2)) - g_accel
    lambda2_bar_dot = np.zeros(X.shape[1])
    lambda4_bar_dot = -X[4]
    # Nondimensionalize time (with tau = t/tf and d/dtau = tf*d/dt). We must
    # multiply each differential equation by tf to convert our derivatives from
    # d/dt to d/dtau.
    dX_dtau = tf*np.array([xdot, ydot, Vxdot, Vydot, lambda2_bar_dot, lambda4_bar_dot])
    return dX_dtau

# Note: this function uses the initial and final boundary conditions and so
# must be defined after those variables are defined
def ascent_bcs_tf(Y0, Yf, tf):
    # Boundary Condition Function for the Flat-Moon Optimal Ascent Problem
    PSI = np.array([
        Y0[0] - x0,
        Y0[1] - y0,
        Y0[2] - Vx0,
        Y0[3] - Vy0,
        Yf[1] - yf,
        Yf[2] - Vxf,
        Yf[3] - Vyf,
    ])
    return PSI

#----------------------------------------------------------------------------
## Initial Guesses
#----------------------------------------------------------------------------
# initial time
t0 = 0
# list initial conditions in yinit, use zero if unknown
yinit = [x0, y0, Vx0, Vy0, 0, 0] # guess for initial state and costate variables
tf_guess = 700 # sec, initial guess for final time

## Page 218

# Because the final time is free, we must parameterize the problem by
# the unknown final time, tf. Create a nondimensional time vector,
# tau, with Nt linearly spaced elements. (tau = time/tf) We will pass the
# unknown final time to solve_bvp as an unknown parameter and the code will
# attempt to solve for the actual final time as it solves our TPBVP.
Nt = 41
tau = np.linspace(0,1,Nt)  # nondimensional time vector
# Create an initial guess of the solution
# Python/scipy do not have an equivalent to bvpinit, so we make our own
# bvpinit basically just stores the mesh and the initial guess inside a struct
# after replicating the initial guess for each point on the mesh. solve_bvp
# takes the mesh as a parameter, so all we need to do is replicate the initial
# guess for each point of the original mesh and make sure the result is nxm, as
# solve_bvp expects, where n is the number of states and m is the mesh size
solinit = np.array([yinit for _ in range(Nt)]).T
#----------------------------------------------------------------------------
## Solution
#----------------------------------------------------------------------------
# Call solve_bvp to solve the TPBVP. Point the solver to the functions
# containing the differential equations and the boundary conditions and
# provide it with the initial guess of the solution.
sol = solve_bvp(ascent_odes_tf, ascent_bcs_tf, tau, solinit, np.array([tf_guess]))
# Extract the final time from the solution:
tf = sol.p[0]
# # Evaluate the solution at all times in the nondimensional time vector tau
# # and store the state variables in the matrix Z.
Z = sol.sol(tau)
# # Convert back to dimensional time for plotting
time = t0 + tau*(tf-t0)
# # Extract the solution for each state variable from the matrix Z:
x_sol = Z[0,:]
y_sol = Z[1,:]
vx_sol = Z[2,:]
vy_sol = Z[3,:]
lambda2_bar_sol = Z[4,:]
lambda4_bar_sol = Z[5,:]

## Plots
fig = plt.figure('Optimal Ascent from Flat Moon')

# Indices are modifed as compared with the code in the book so that the
# plots appears in the same order as in the book
ax = fig.add_subplot(323)
ax.plot(time, x_sol/1000)
ax.set_ylabel('x, km')
ax.set_xlabel('Time, sec')
ax.grid()

ax = fig.add_subplot(324)
ax.plot(time, y_sol/1000)
ax.set_ylabel('y, km')
ax.set_xlabel('Time, sec')
ax.grid()

ax = fig.add_subplot(325)
ax.plot(time, vx_sol/1000)
ax.set_ylabel('V_x, km')
ax.set_xlabel('Time, sec')
ax.grid()

ax = fig.add_subplot(326)
ax.plot(time, vy_sol/1000)
ax.set_ylabel('V_y, km')
ax.set_xlabel('Time, sec')
ax.grid()

ax = fig.add_subplot(321)
ax.plot(time, np.rad2deg(np.arctan(lambda4_bar_sol)))
ax.set_ylabel(r'$\alpha$ deg')
ax.set_xlabel('Time, sec')
ax.grid()

ax = fig.add_subplot(322)
ax.plot(x_sol/1000, y_sol/1000)
ax.set_ylabel('y, km')
ax.set_xlabel('x, km')
ax.grid()
