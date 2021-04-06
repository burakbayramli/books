# We consider the steady, one dimensional row of a constant-density fluid through a duct with constant area
# We use the staggered grid. Density 1.0 kg/m3 is constant. Duct area A is constant. d = 1.0
# Boundary conditions: u1 = 10 m/s and P_boundary = 0 Pa.

import numpy as np
from matplotlib import pyplot as plt

# Define parameters
x_len = 1
x_points = 50
del_x = x_len/float(x_points-1)

rho = 1.0
u0 = 10.0
d = 1.0
A = 1.0     # Constant area of cross-section
P_boundary = 0

# Define the staggered grid
x_gr = np.linspace(0+(del_x/float(2)), x_len, x_points-1, dtype = float)        # grid points (pressure)
x_cv = np.linspace(0, x_len+(del_x/float(2)), x_points, dtype = float)          # Control volume points (velocity)

P_coeff_mat = np.zeros((len(x_gr), len(x_gr)))
P_sol_mat = np.zeros(len(x_gr))

u_star = np.random.choice(np.arange(1,100), size=len(x_cv))   # Guessed velocities at nodes defined by random integers
# u_star = np.ones(len(x_cv))*10
u_star[0] = u0
print ("\n Guessed velocities at the nodes (randomly assigned) \n" + str(u_star))

# For interior nodes
for i in range(1, x_points-2):
    aW = rho*d*A
    aE = rho*d*A
    aP = aW + aE
    b = (rho*u_star[i]*A) - (rho*u_star[i+1]*A)

    P_coeff_mat[i,i] = aP
    P_coeff_mat[i, i + 1] = -1.0 * aE
    P_coeff_mat[i, i - 1] = -1.0 * aW

    P_sol_mat[i] = b

# For boundary node 0
aW = 0.0
aE = rho*d*A
aP = aW + aE
b = (rho*u_star[0]*A) - (rho*u_star[1]*A)
P_coeff_mat[0,0] = aP
P_coeff_mat[0,1] = -1.0* aE
P_sol_mat[0] = b

# For boundary node -1
P_coeff_mat[-1,-1] = 1
P_sol_mat[-1] = P_boundary

print ("\n The coefficient matrix for pressure correction is: \n" + str(P_coeff_mat))
print ("\n The solution matrix for pressure correction is: \n" + str(P_sol_mat))


# Calculate the pressure correction terms at the grid points
P_corr = np.linalg.solve(P_coeff_mat, P_sol_mat)
print ("\n The pressure correction terms are: \n" + str(P_corr))

# Calculate velocity at CV points
u = np.zeros(len(x_cv)-1)
u[0] = u0
for i in range(1, len(x_cv)-1):
    u[i] = u_star[i] + d*(P_corr[i-1] - P_corr[i])

print ("\n The velocity profile is: \n" + str(u))
print ("\n In this very straightforward problem with constant area and constant density, the "
       "velocity must be constant everywhere by continuity.\n")

plt.plot(u)
plt.show()
