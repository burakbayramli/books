# The lid-driven cavity problem has long been used a test or validation case for new codes or new solution methods.
# The standard case is fluid contained in a square domain with Dirichlet boundary conditions on all sides, with three
# stationary sides and one moving side (with velocity tangent to the side).

import numpy as np
from matplotlib import pyplot as plt

# Define parameters
x_len = 1.0
y_len = 1.0
x_points = 101
y_points = 101
del_x = x_len/float(x_points)
del_y = y_len/float(y_points)
del_t = 0.001                       # Keep small time steps
num_itrs = 5100
delta = 4.5
Re = 100
U_boundary = 1.0

u = np.zeros((x_points, y_points+1))
v = np.zeros((x_points+1, y_points))
p = np.ones((x_points+1, y_points+1))
u[:,-1] = U_boundary
u[:,-2] = U_boundary

u_new = np.zeros((x_points, y_points+1))
v_new = np.zeros((x_points+1, y_points))
p_new = np.ones((x_points+1, y_points+1))

for itr in range(num_itrs):
    for i in range(1, x_points-1):  # u momentum equations
        for j in range(1, y_points):
            u_new[i,j] = u[i,j] - del_t*((u[i+1,j]*u[i+1,j] - u[i-1,j]*u[i-1,j])/float(2.0*del_x)) - del_t*((((u[i,j] + u[i,j+1])*(v[i,j] + v[i+1,j])) - ((u[i,j] + u[i,j-1])*(v[i,j-1] + v[i+1,j-1])))/float(4.0*del_y)) - del_t*(p[i+1,j] - p[i,j])/float(del_x) + del_t*((u[i+1,j] + u[i-1,j] - 2.0*u[i,j])/float(del_x*del_x) + (u[i,j+1] + u[i,j-1] - 2.0*u[i,j])/float(del_y*del_y))/float(Re)

    u_new[0,:] = 0                  # boundary condition: u at the left wall = 0
    u_new[-1,:] = 0                 # boundary condition: u at the right wall = 0
    u_new[:, 0] = -1.0 * u_new[:, 1]    # boundary condition: average u at the bottom surface = 0
    u_new[:,-1] = 2*U_boundary - u_new[:,-2]    # boundary condition: average u at the top surface = U_boundary

    for i in range(1, x_points):  # v momentum equations
        for j in range(1, y_points-1):
            v_new[i,j] = v[i,j] - del_t*((v[i,j+1]*v[i,j+1] - v[i,j-1]*v[i,j-1])/float(2.0*del_y)) - del_t*((((u[i,j] + u[i,j+1])*(v[i,j] + v[i+1,j])) - ((u[i-1,j] + u[i-1,j+1])*(v[i,j] + v[i-1,j])))/float(4.0*del_x)) - del_t*(p[i,j+1] - p[i,j])/float(del_y) + del_t*((v[i+1,j] + v[i-1,j] - 2.0*v[i,j])/float(del_x*del_x) + (v[i,j+1] + v[i,j-1] - 2.0*v[i,j])/float(del_y*del_y))/float(Re)

    v_new[0,:] = -1.0 * v_new[1,:]      # boundary condition: average v at the left wall = 0
    v_new[-1,:] = -1.0 * v_new[-2,:]    # boundary condition: average v at the right wall = 0
    v_new[:, 0] = 0                     # boundary condition: v at the bottom surface = 0
    v_new[:,-1] = 0                     # boundary condition: v at the top surface = 0

    for i in range(1, x_points):  # continuity equations
        for j in range(1, y_points):
            p_new[i,j] = p[i,j] - delta*del_t*(((u_new[i,j] - u_new[i-1,j])/float(del_x) + (v_new[i,j] - v_new[i,j-1])/float(del_y)))

    p_new[0,:] = p_new[1,:]             # boundary condition: average P at the left wall = 0
    p_new[-1,:] = p_new[-2,:]           # boundary condition: average P at the right wall = 0
    p_new[:,0] = p_new[:,1]             # boundary condition: average P at the bottom surface = 0
    p_new[:,-1] = p_new[:,-2]           # boundary condition: average P at the top surface = 0

    for i in range(x_points):     # passing current value array to old value array
        for j in range(y_points+1):
            u[i,j] = u_new[i,j]

    for i in range(x_points+1):
        for j in range(y_points):
            v[i,j] = v_new[i,j]

    for i in range(x_points+1):
        for j in range(y_points+1):
            p[i,j] = p_new[i,j]

print("\n U velocity is: \n" + str(u_new))
print("\n V velocity is: \n" + str(v_new))
print("\n Pressure distribution is: \n" + str(p_new))

x_u = np.linspace(0, x_len, x_points+1)
y_u = np.linspace(0, y_len, y_points)
plt.contourf(y_u, x_u, u_new)
plt.colorbar()
plt.xlabel("x")
plt.ylabel("y")
plt.title("U velocity contour")
plt.show()

x_v = np.linspace(0, x_len, x_points)
y_v = np.linspace(0, y_len, y_points+1)
plt.contourf(y_v, x_v, v_new)
plt.colorbar()
plt.xlabel("x")
plt.ylabel("y")
plt.title("V velocity contour")
plt.show()

x_p = np.linspace(0, x_len, x_points+1)
y_p = np.linspace(0, y_len, y_points+1)
plt.contourf(y_p, x_p, p_new)
plt.colorbar()
plt.xlabel("x")
plt.ylabel("y")
plt.title("Pressure contour")
plt.show()
