# Consider convection and diffusion in the one-dimensional domain. Calculate the transient temperature field if the
# initial temperature is zero everywhere and the boundary conditions are phi = 0 at x = 0 and dphi/dx = 0 at x = L.
# The data are L = 1.5 m, u = 2 m/s, rho = 1.0 kg/m3 and Gamma = 0.03 kg/m.s. The source distribution defined with
# a = -200, b = 100, x1 = 0.6 m, x2 = 0.2 m.

import numpy as np
from matplotlib import pyplot as plt


# Define D - diffusion conductance
def D(x):
    return Gamma/float(x)


# Define F - convection strength
def F(vel):
    return rho*vel


# Define the parameters
rho = 1.0
u = 2.0
Gamma = 0.03
phi_0 = 12       # Initial flux (boundary condition)

# Define the domain
x_len = 1.5
x_points = 45
del_x = x_len/float(x_points-1)
x_gr = np.linspace(0, x_len, x_points)        # Co-ordinates of grid points

del_t = 0.01           # Time step

# Calculate the parameters at the nodes
phi = np.zeros(x_points)
phi_old = np.ones(x_points) * phi_0
num_itrs = 20       # Number of iterations

coeff_mat = np.zeros((x_points, x_points))
sol_mat = np.zeros(x_points)

for itrs in range(num_itrs):
    for i in range(2, x_points-1):
        aW = D(del_x/float(2)) + F(u)
        aE = D(del_x/float(2))
        aP_old = rho * del_x / float(del_t)
        Su = (1 / float(8)) * (F(u) * (3 * phi[i] - 2 * phi[i - 1] - phi[i - 2])) + (1 / float(8)) * (
                    F(u) * (phi[i - 1] + 2 * phi[i] - 3 * phi[i + 1]))
        Sp = 0
        aP = aW + aE + aP_old - Sp + (F(u) - F(u))
        coeff_mat[i, i] = aP
        coeff_mat[i, i + 1] = -1.0 * aE
        coeff_mat[i, i - 1] = -1.0 * aW
        sol_mat[i] = Su + aP_old*phi_old[i]

    # Boundary node 0
    aW = 0
    aE = D(del_x / float(2)) + (D(del_x))/float(3.0)
    aP_old = rho * del_x / float(del_t)
    Su = phi_0*(F(u) + (8/float(3))*D(del_x)) + (1/float(8))*F(u)*(phi[0] - 3*phi[1])
    Sp = -1.0 * (phi_0*(F(u) + (8/float(3.0))*D(del_x)))
    aP = aW + aE + aP_old - Sp + (F(u) - F(u))
    coeff_mat[0,0] = aP
    coeff_mat[0,1] = -1.0 * aE
    sol_mat[0] = Su + aP_old * phi_old[0]

    # Boundary node 1
    aW = D(del_x/float(2)) + F(u)
    aE = D(del_x/float(2))
    aP_old = rho * del_x / float(del_t)
    Su = (1 / float(8)) * (F(u) * (3 * phi[1] - phi[0])) + (1 / float(8)) * (
            F(u) * (phi[0] + 2 * phi[1] - 3 * phi[2]))
    Sp = 0
    aP = aW + aE + aP_old - Sp + (F(u) - F(u))
    coeff_mat[1, 1] = aP
    coeff_mat[1, 2] = -1.0 * aE
    coeff_mat[1, 0] = -1.0 * aW
    sol_mat[1] = Su + aP_old * phi_old[1]

    # Boundary node L
    aE = 0
    aW = D(del_x/float(2)) + F(u)
    aP_old = rho * del_x / float(del_t)
    Su = (1 / float(8)) * (F(u) * (3 * phi[-1] - 2 * phi[-2] - phi[-3]))
    Sp = 0
    aP = aW + aE + aP_old - Sp + (F(u) - F(u))
    coeff_mat[-1,-1] = aP
    coeff_mat[-1, -2] = -1.0 * aW
    sol_mat[-1] = Su + aP_old * phi_old[-1]

    phi = np.linalg.solve(coeff_mat, sol_mat)

    for i in range(len(phi)):
        phi_old[i] = phi[i]

print ("\n The temperature distribution is: \n" + str(phi))

plt.plot(x_gr, phi)
plt.xlabel("Length")
plt.ylabel("Flux")
plt.title("Implicit + QUICK Scheme")
plt.show()
