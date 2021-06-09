# Anderson 9.3.2

import numpy as np
from matplotlib import pyplot as plt

# Define the domain
y_len = 1
y_points = 21
del_y = y_len/float(y_points-1)
y_gr = np.linspace(0,y_len, y_points)
u = np.zeros(len(y_gr))
u[-1] = 1       # u of the upper surface = u of the moving plate = 1 m/s

Re = 5000       # Reynolds number
del_t = Re*del_y*del_y
K = np.zeros(len(y_gr)-1)
coeff_mat = np.zeros((len(y_gr)-1, len(y_gr)-1))

num_itrs = 200
for itrs in range(num_itrs):
    for i in range(1, y_points-2):
        A = -1.0 * del_t/float(2.0* Re * del_y * del_y)
        B = 1.0 + (del_t/float(Re * del_y * del_y))

        coeff_mat[i,i] = B
        coeff_mat[i, i+1] = A
        coeff_mat[i, i-1] = A
        K[i] = (1.0 - (del_t / float(Re * del_y * del_y))) * u[i + 1] + (del_t / float(2 * Re * del_y * del_y)) * (
                    u[i + 2] + u[i])

    A = -1.0 * del_t / float(2.0 * Re * del_y * del_y)
    B = 1.0 + (del_t / float(Re * del_y * del_y))
    coeff_mat[0,0] = B
    coeff_mat[0,1] = A
    K[0] = (1.0 - (del_t / float(Re * del_y * del_y))) * u[1] + (del_t / float(2 * Re * del_y * del_y)) * (
                    u[2] + u[0])

    coeff_mat[-1,-1] = B
    coeff_mat[-1,-2] = A
    K[-1] = ( (1.0 - (del_t / float(Re * del_y * del_y))) * u[-1] + (del_t / float(2 * Re * del_y * del_y)) * (
                    u[-1] + u[-3]) ) - A*u[-1]

    # Solve for u
    u_sol = np.linalg.solve(coeff_mat, K)

    # Update u_sol to u
    for i in range(1, len(y_gr)-1):
        u[i] = u_sol[i-1]

print ("\n Value of u is: \n" + str(u))
plt.plot(u, y_gr)
plt.show()
