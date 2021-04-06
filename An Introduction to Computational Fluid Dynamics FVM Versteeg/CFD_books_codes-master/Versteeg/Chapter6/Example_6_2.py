# We consider a planar nozzle. Flow is steady and frictionless. Using SIMPLE write down the discretised momentum and
# pressure correction equations and solve for the unknown pressures and velocities at nodes.
# Density is 1 kg/m3. Nozzle length is 2 m.
# Cross-sectional area at the inlet is 0.5 m2, and exit is 0.1 m2. (linear function of distance)
# Initial velocity field: Guess a mass flow rate is 1 kg/sec
# Initial pressure field:  assume a linear pressure variation, inlet 10 Pa and exit 0 Pa


import numpy as np
from matplotlib import pyplot as plt

# Define parameters
x_len = 2
x_points = 50
del_x = x_len/float(x_points-1)

rho = 1.0
mass_flow = 1.0

# Define the staggered grid
x_gr = np.linspace(0, x_len, x_points, dtype = float)        # grid points (pressure)
x_cv = np.linspace(0, x_len-(del_x/float(2)), x_points-1, dtype = float)          # Control volume points (velocity)

A_gr = np.linspace(0.5, 0.1, len(x_gr), dtype=float)         # linearly varying area of cross-section
A_cv = np.linspace(0.45, 0.15, len(x_cv), dtype=float)

u0 = np.zeros(len(x_cv))                                      # Initial guess velocity field at CV faces
for i in range(len(x_cv)):
    u0[i] = mass_flow / float(rho * A_cv[i])

P0 = np.linspace(10, 0, len(x_gr), dtype=float)              # Initial guess pressure at grids

# Discretized momentum equation (velocity field guess)
u_star = np.zeros(len(x_cv))
d = np.ones(len(x_cv))
u_coeff_mat = np.zeros((len(x_cv), len(x_cv)))
u_sol_mat = np.zeros(len(x_cv))

# For interior nodes
for i in range(1, len(x_cv)-1):
    Fw = rho * ((u0[i - 1] + u0[i]) / float(2)) * A_gr[i]
    Fe = rho * ((u0[i] + u0[i + 1]) / float(2)) * A_gr[i+1]
    aW = Fw
    aE = 0
    aP = aW + aE + (Fe - Fw)
    Su = (P0[i] - P0[i+1])*A_cv[i]
    d[i] = A_cv[i]/float(aP)

    u_coeff_mat[i,i] = aP
    u_coeff_mat[i,i+1] = -1.0 * aE
    u_coeff_mat[i,i-1] = -1.0 * aW
    u_sol_mat[i] = Su

# For boundary node 0
Fw = rho* u0[0]* A_gr[0]
Fe = rho * ((u0[0] + u0[1]) / float(2)) * A_gr[1]
aW = 0
aE = 0
aP = Fe + Fw*(pow((A_cv[0]/float(A_gr[0])), 2))/float(2)
Su = (P0[0] - P0[1])*A_cv[0] + Fw*(A_cv[0]/float(A_gr[0]))*u0[0]
u_coeff_mat[0,0] = aP
u_coeff_mat[0,1] = -1.0 * aE
u_sol_mat[0] = Su
d[0] = A_cv[0]/float(aP)

# For boundary node -1
Fw = rho * ((u0[-2] + u0[-1]) / float(2)) * A_gr[-2]
Fe = rho* u0[-1]* A_cv[-1]
aW = Fw
aE = 0
aP = aW + aE + (Fe - Fw)
Su = (P0[-2] - P0[-1])*A_cv[-1]
u_coeff_mat[-1,-1] = aP
u_coeff_mat[-1,-2] = -1.0 * aW
u_sol_mat[-1] = Su
d[-1] = A_cv[-1]/float(aP)

print ("\n The coefficient matrix for velocity momentum equations is: \n" + str(u_coeff_mat))
print ("\n The solution matrix for velocity momentum equations is: \n" + str(u_sol_mat))

# Calculate the pressure correction terms at the grid points
u_guess = np.linalg.solve(u_coeff_mat, u_sol_mat)
print ("\n The velocity momentum equations terms are: \n" + str(u_guess))

# Pressure correction
P_coeff_mat = np.zeros((len(x_gr), len(x_gr)))
P_sol_mat = np.zeros(len(x_gr))

# For interior nodes
for i in range(1, len(x_gr)-1):
    aW = rho*d[i-1]*A_cv[i-1]
    aE = rho*d[i]*A_cv[i]
    Fw = rho * u_guess[i - 1] * A_cv[i - 1]
    Fe = rho * u_guess[i] * A_cv[i]
    aP = aW + aE
    b = Fw - Fe

    P_coeff_mat[i,i] = aP
    P_coeff_mat[i, i + 1] = -1.0 * aE
    P_coeff_mat[i, i - 1] = -1.0 * aW

    P_sol_mat[i] = b

# For boundary node 0
P_coeff_mat[0,0] = 1
P_sol_mat[0] = 0            # Since pressure correction at nodes is 0 (because statc pressure given at nodes)

# For boundary node -1
P_coeff_mat[-1,-1] = 1
P_sol_mat[-1] = 0           # Since pressure correction at nodes is 0 (because statc pressure given at nodes)

print ("\n The coefficient matrix for pressure correction is: \n" + str(P_coeff_mat))
print ("\n The solution matrix for pressure correction is: \n" + str(P_sol_mat))


# Calculate the pressure correction terms at the grid points
P_corr = np.linalg.solve(P_coeff_mat, P_sol_mat)
print ("\n The pressure correction terms are: \n" + str(P_corr))

# Calculate velocity at CV points
u = np.zeros(len(x_cv))
for i in range(0, len(x_cv)):
    u[i] = u_guess[i] + d[i]*(P_corr[i] - P_corr[i+1])

print ("\n The velocity profile is: \n" + str(u))

plt.plot(x_cv, u)
plt.show()
