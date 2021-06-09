# A solenoidal velocity field produces a pattern of streamlines
# https://pdfs.semanticscholar.org/7a2e/0e56e20f165507f24cb611ee0e2827b85118.pdf - Section: 3.2.1

import numpy as np
from matplotlib import pyplot as plt
from matplotlib import cm
import math


def D(dist):
    return gamma/float(dist)


def F(vel):
    return rho*vel


# Define the domain
x_len = 2
y_len = 1
x_points = 50
y_points = 25
del_x = x_len/float(x_points-1)
del_y = y_len/float(y_points-1)
x_gr = np.linspace(-1, 1, x_points)
y_gr = np.linspace(0, 1, y_points)

# Define the parameters
t_steps = 50
del_t = 0.001

u = np.zeros((x_points, y_points))
v = np.zeros((x_points, y_points))
for i in range(x_points):
    for j in range(y_points):
        u[i, j] = 2.0 * y_gr[j] * (1.0 - (x_gr[i] * x_gr[i]))
        v[i, j] = -2.0 * x_gr[i] * (1.0 - (y_gr[j] * y_gr[j]))

alp = 0.01
phi = np.ones((x_points, y_points))*(1 - math.tanh(alp))
for i in range(x_points):
    if x_gr[i] < 0:
        phi[i, 0] = 1.0 + math.tanh(alp*(2.0*x_gr[i] + 1))
    else:
        phi[i, 0] = phi[i, 1]

rho = 1.0
Pe = 100
gamma = rho/float(Pe)

# Iteration
fig = plt.figure()
ax1 = fig.add_subplot(1, 1, 1)
line1 = ax1.pcolormesh(y_gr, x_gr, phi, cmap = cm.jet)

plt.ion()
plt.show()

for it in range(t_steps):

    # Internal points
    coeff_mat = np.zeros((y_points, y_points))
    sol_mat = np.zeros(y_points)
    for i in range(1, x_points-1):
        for j in range(1, y_points-1):
            aW = D(del_x) - 0.5 * F(u[i-1, j])
            aE = D(del_x) + 0.5 * F(u[i+1, j])
            aN = D(del_y) - 0.5 * F(v[i, j+1])
            aS = D(del_y) + 0.5 * F(v[i, j-1])
            Sp = -rho*del_x*del_y/float(del_t)
            Su = rho*del_x*del_y*phi[i,j]/float(del_t)
            aP = aW + aE + aN + aS - Sp

            coeff_mat[j,j] = aP
            coeff_mat[j, j + 1] = -1.0 * aE
            coeff_mat[j, j - 1] = -1.0 * aW
            sol_mat[j] = Su

            # Bottom node
        if x_gr[i] < 0:
            coeff_mat[0, 0] = 1
            sol_mat[0] = 1.0 + math.tanh(alp * (2.0 * x_gr[i] + 1))
        else:
            coeff_mat[0, 0] = 1
            sol_mat[0] = sol_mat[1]

            # Top node
        coeff_mat[-1,-1] = 1
        sol_mat[-1] = 1.0 - math.tanh(alp)

        # Solve for internal nodes
        phi[i, :] = np.linalg.solve(coeff_mat, sol_mat)

    # Left boundary points
    phi[0, :] = 1.0 - math.tanh(alp)

        # Top node
    phi[0, -1] = 1.0 - math.tanh(alp)

        # Bottom node
    phi[0, 0] = 1.0 + math.tanh(alp*(2.0*x_gr[0] + 1))


    # Right boundary points
    phi[-1, :] = 1.0 - math.tanh(alp)

        # Top node
    phi[-1, -1] = 1.0 - math.tanh(alp)

        # Bottom node
    phi[-1, 0] = phi[-2, 0]

    # Updating plots
    line1 = ax1.pcolormesh(y_gr, x_gr, phi, cmap = cm.jet)
    plt.gcf().canvas.draw()

print("\n Phi distribution is : \n" + str(phi))

plt.ioff()
plt.show()

plt.pcolormesh(y_gr, x_gr, phi, cmap = cm.jet)
plt.colorbar()
plt.streamplot(y_gr, x_gr, v, u, color='k', linewidth=0.7)
plt.title("phi distribution for time interation " + str(t_steps))
plt.savefig("phi_timestep_" + str(t_steps) + ".png")