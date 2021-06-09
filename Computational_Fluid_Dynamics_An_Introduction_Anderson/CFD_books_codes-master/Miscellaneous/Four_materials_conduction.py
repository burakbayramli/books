# The Four materials problem is a benchmark heat conduction problem in transient regime proposed by the CTTC (2016)
# https://pdfs.semanticscholar.org/7a2e/0e56e20f165507f24cb611ee0e2827b85118.pdf (Section 2.2)

import numpy as np
from matplotlib import pyplot as plt
from matplotlib import cm

# Define the domain
x_len = 1.1
y_len = 0.8
x_points = 21
y_points = 17
del_x = x_len/float(x_points-1)
del_y = y_len/float(y_points-1)
x_gr = np.linspace(0, x_len, x_points)
y_gr = np.linspace(0, y_len, y_points)

# Define the parameters
t_steps = 100
del_t = 1

rho = np.zeros((x_points, y_points))
Cp = np.zeros((x_points, y_points))
lam = np.zeros((x_points, y_points))

for i in range(x_points):
    for j in range(y_points):
        if x_gr[i] < 0.5 and y_gr[j] < 0.4:
            rho[i,j] = 1500
            Cp[i,j] = 750
            lam[i,j] = 170
        elif x_gr[i] >= 0.5 and y_gr[j] < 0.7:
            rho[i, j] = 1600
            Cp[i, j] = 770
            lam[i, j] = 140
        elif x_gr[i] < 0.5 and y_gr[j] >= 0.4:
            rho[i, j] = 1900
            Cp[i, j] = 810
            lam[i, j] = 200
        else:
            rho[i, j] = 2500
            Cp[i, j] = 930
            lam[i, j] = 140

Temp = np.zeros((x_points, y_points))
Temp_new = np.zeros((x_points, y_points))
T_bott = 23
Q_top = 60
T_f = 33
alp = 9
Temp[:,0] = T_bott
Temp[-1, :] = 8

# Iterations
coeff_mat = np.zeros((y_points, y_points))
sol_mat = np.zeros(y_points)
for it in range(t_steps):
    for i in range(1, x_points-1):
        for j in range(1, y_points-1):
            aW = lam[i, j] * del_y / float(0.5 * del_x)
            aE = lam[i, j] * del_y / float(0.5 * del_x)
            aN = lam[i, j] * del_x / float(0.5 * del_y)
            aS = lam[i, j] * del_x / float(0.5 * del_y)
            Sp = - rho[i, j] * Cp[i, j] * del_x * del_y / float(del_t)
            Su = rho[i, j] * Cp[i, j] * del_x * del_y * Temp[i,j] / float(del_t)
            aP = aW + aE + aN + aS - Sp

            coeff_mat[j, j] = aP
            coeff_mat[j, j + 1] = -1.0 * aE
            coeff_mat[j, j - 1] = -1.0 * aW
            sol_mat[j] = Su

        # Bottom point
        aW = 0
        aE = 0
        aN = 0
        aS = 0
        aP = 1
        coeff_mat[0,0] = aP
        sol_mat[0] = T_bott

        # Top point
        aW = lam[i, -1] * del_y / float(0.5 * del_x)
        aE = 0
        aN = 0
        aS = lam[i, -1] * del_x / float(0.5 * del_y)
        Sp = - rho[i, -1] * Cp[i, -1] * del_x * del_y / float(del_t) - alp * del_y
        Su = rho[i, -1] * Cp[i, -1] * del_x * del_y * Temp[i,-1] / float(del_t) + Q_top*del_x
        aP = aW + aE + aN + aS - Sp
        coeff_mat[-1, -1] = aP
        coeff_mat[-1, -2] = -1.0* aW
        sol_mat[-1] = Su

        Temp[i,:] = np.linalg.solve(coeff_mat, sol_mat)

    # Left boundary
    for j in range(1, y_points-1):
        aW = 0
        aE = lam[0, j] * del_y / float(0.5 * del_x)
        aN = lam[0, j] * del_y / float(0.5 * del_y)
        aS = lam[0, j] * del_x / float(0.5 * del_y)
        Sp = - rho[0, j] * Cp[0, j] * del_x * del_y / float(del_t) - alp * del_y
        Su = rho[0, j] * Cp[0, j] * del_x * del_y * Temp[0, j] / float(del_t) + alp * del_y * T_f
        aP = aW + aE + aN + aS - Sp

        coeff_mat[j, j] = aP
        coeff_mat[j, j + 1] = -1.0 * aE
        coeff_mat[j, j - 1] = -1.0 * aW
        sol_mat[j] = Su

    # Bottom point
    aW = 0
    aE = 0
    aN = 0
    aS = 0
    aP = 1
    coeff_mat[0, 0] = aP
    sol_mat[0] = T_bott

    # Top point
    aW = lam[0, -1] * del_y / float(0.5 * del_x)
    aE = 0
    aN = 0
    aS = lam[0, -1] * del_x / float(0.5 * del_y)
    Sp = - rho[0, -1] * Cp[0, -1] * del_x * del_y / float(del_t) - alp * del_y
    Su = rho[0, -1] * Cp[0, -1] * del_x * del_y * Temp[0, -1] / float(del_t) + Q_top * del_x
    aP = aW + aE + aN + aS - Sp
    coeff_mat[-1, -1] = aP
    coeff_mat[-1, -2] = -1.0* aW
    sol_mat[-1] = Su

    Temp[0, :] = np.linalg.solve(coeff_mat, sol_mat)

    # Right boundary
    Temp[-1, :] = 8 + 0.005*it

print ("\n Temperature distribution is : \n" + str(Temp))
plt.contourf(y_gr, x_gr, Temp, cmap = cm.jet)
plt.colorbar()
plt.title("Temperature distribution for timestep: " + str(t_steps))
plt.savefig("Temp_dist.png")
plt.show()
