# A thin plate is initially at a uniform temperature of 200 C. At a certain time t = 0 the temperature of the east side
# of the plate is suddenly reduced to 0 C. The other surface is insulated. Use the implicit finite volume method in
# conjunction with a suitable time step size to calculate the transient temperature distribution of the slab.
# Plate thickness L = 2 cm, thermal conductivity k = 10 W/mK and rho*c = 10 x 106 J/m3K

import numpy as np
from matplotlib import pyplot as plt

# Define the parameters
k = 10
rho_c = pow(10,7)
T_init = 200    # Initial temperature
T_L = 0         # At x= L, Boundary temperature

# Define the domain
x_len = 0.02
x_points = 40
del_x = x_len/float(x_points-1)
x_gr = np.linspace(0, x_len, x_points)        # Co-ordinates of grid points

del_t = 2           # Time step

# Calculate the parameters at the nodes
Temp = np.zeros(x_points)
Temp_old = np.ones(x_points) * T_init
num_itrs = 20       # Number of iterations

coeff_mat = np.zeros((x_points, x_points))
sol_mat = np.zeros(x_points)

for itrs in range(num_itrs):
    for i in range(1, x_points-1):
        aW = k / float(del_x)
        aE = k / float(del_x)
        aP_old = rho_c * del_x / float(del_t)
        Su = 0
        Sp = 0
        aP = aW + aE + aP_old - Sp
        coeff_mat[i, i] = aP
        coeff_mat[i, i + 1] = -1.0 * aE
        coeff_mat[i, i - 1] = -1.0 * aW
        sol_mat[i] = Su + aP_old*Temp_old[i]

    # Boundary node 0
    aW = 0
    aE = k / float(del_x)
    aP_old = rho_c * del_x / float(del_t)
    Su = 0
    Sp = 0
    aP = aW + aE + aP_old - Sp
    coeff_mat[0,0] = aP
    coeff_mat[0, 1] = -1.0 * aE
    sol_mat[0] = Su + aP_old * Temp_old[0]

    # Boundary node L
    aE = 0
    aW = k / float(del_x)
    aP_old = rho_c * del_x / float(del_t)
    Su = (2.0 * k / float(del_x)) * T_L
    Sp = -2.0 * k / float(del_x)
    aP = aW + aE + aP_old - Sp
    coeff_mat[-1, -1] = aP
    coeff_mat[-1, -2] = -1.0 * aW
    sol_mat[-1] = Su + aP_old * Temp_old[-1]

    Temp = np.linalg.solve(coeff_mat, sol_mat)

    for i in range(len(Temp)):
        Temp_old[i] = Temp[i]

print ("\n The temperature distribution is: \n" + str(Temp))

plt.plot(x_gr, Temp)
plt.xlabel("Length")
plt.ylabel("Temperature")
plt.title("Implicit scheme")
plt.show()
