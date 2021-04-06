# http://cav2012.sg/cdohl/CFD_course/Raizan%2520FDM%2520obstacle.html

import numpy as np
from matplotlib import pyplot as plt

# Define the domain
x_len = 20
y_len = 10
x_points = 201
y_points = 101
del_x = x_len/float(x_points-1)
del_y = y_len/float(y_points-1)
x_gr = np.linspace(0, x_len, x_points)
y_gr = np.linspace(0, y_len, y_points)

# Define the obstacle
x_obs = 40       # bottom surface position (grid point number)
wdth = 5        # width of the obstacle (number of grid points)
dpth = 20       # height of the obstacle (number of grid points)
y_obs = 70        # left wall position (grid point number)

# Define parameters
Re = 50
del_t = 0.0001
rho = 1
u_0 = 1
nu = del_y*wdth*rho*u_0/float(Re)
num_itrs = 200

u = np.ones((x_points, y_points))*u_0
v = np.zeros((x_points, y_points))
P = np.zeros((x_points, y_points))

u_new = np.ones((x_points, y_points))*u_0
v_new = np.zeros((x_points, y_points))
P_new = np.zeros((x_points, y_points))

# Iterations
for it in range(num_itrs):
    for i in range(1, x_points-1):
        for j in range(1, y_points-1):
            P_new[i,j] = ((P[i+1,j]+P[i-1,j])*del_y**2+(P[i,j+1]+P[i,j-1])*del_x**2)/(float(2*(del_x**2+del_y**2))) -del_x**2*del_y**2/(float(2*(del_x**2+del_y**2)))*(rho*(1/float(del_t)*((u[i+1,j]-u[i-1,j])/(float(2*del_x))+(v[i,j+1]-v[i,j-1])/(float(2*del_y)))-((u[i+1,j]-u[i-1,j])/(float(2*del_x)))**2-2*((u[i,j+1]-u[i,j-1])/(float(2*del_y))*(v[i+1,j]-v[i-1,j])/(float(2*del_x)))-((v[i,j+1]-v[i,j-1])/(float(2*del_y)))**2))

    # Pressure boundary conditions
    P_new[0, :] = P_new[1, :] - (rho * nu / float(del_y)) * (-2 * v[1, :] + v[2, :])  # y = 0 wall
    P_new[-1, :] = P_new[-2, :] - (rho * nu / float(del_y)) * (-2 * v[-2, :] + v[-3, :])  # y = L wall
    P_new[:, 0] = P_new[:, 1] - (rho * nu / float(del_x)) * (-2 * u[:, 1] + u[:, 2])  # x = 0 surface
    P_new[:, -1] = P_new[:, -2] - (rho * nu / float(del_x)) * (-2 * u[:, -2] + u[:, -3])  # x = L surface

    P_new[x_obs, y_obs:(y_obs+dpth+1)] = P_new[x_obs-1, y_obs:(y_obs+dpth+1)] - (rho * nu / float(del_y)) * (-2 * v[x_obs-1, y_obs:(y_obs+dpth+1)] + v[x_obs-2, y_obs:(y_obs+dpth+1)])  # left surface of obstacle
    P_new[(x_obs+wdth), y_obs:(y_obs+dpth+1)] = P_new[(x_obs+wdth+1), y_obs:(y_obs+dpth+1)] - (rho * nu / float(del_y)) * (-2 * v[(x_obs+wdth+1), y_obs:(y_obs+dpth+1)] + v[(x_obs+wdth+2), y_obs:(y_obs+dpth+1)])  # right surface of obstacle
    P_new[x_obs:(x_obs+wdth+1), y_obs] = P_new[x_obs:(x_obs+wdth+1), y_obs-1] - (rho * nu / float(del_x)) * (-2 * u[x_obs:(x_obs+wdth+1), y_obs-1] + u[x_obs:(x_obs+wdth+1), y_obs-2])  # bottom surface of obstacle
    P_new[x_obs:(x_obs+wdth+1), (y_obs+dpth)] = P_new[x_obs:(x_obs+wdth+1), (y_obs+dpth+1)] - (rho * nu / float(del_x)) * (-2 * u[x_obs:(x_obs+wdth+1), (y_obs+dpth+1)] + u[x_obs:(x_obs+wdth+1), (y_obs+dpth+2)])  # top surface of obstacle

    P_new[(x_obs+1):(x_obs+wdth), (y_obs+1):(y_obs+dpth)] = 0   # Pressure in the obstacle is 0

    for i in range(1, x_points-1):
        for j in range(1, y_points-1):
            u_new[i,j] = u[i,j]-u[i,j]*del_t/float(del_x)*(u[i,j]-u[i-1,j])-v[i,j]*del_t/float(del_y)*(u[i,j]-u[i,j-1])-del_t/(float(2*rho*del_x))*(P_new[i+1,j]-P_new[i-1,j])+nu*(del_t/float(del_x)**2*(u[i+1,j]-2*u[i,j]+u[i-1,j])+del_t/float(del_y)**2*(u[i,j+1]-2*u[i,j]+u[i,j-1]))
            v_new[i,j] = v[i,j]-u[i,j]*del_t/float(del_x)*(v[i,j]-v[i-1,j])-v[i,j]*del_t/float(del_y)*(v[i,j]-v[i,j-1])-del_t/(float(2*rho*del_y))*(P_new[i,j+1]-P_new[i,j-1])+nu*(del_t/float(del_x)**2*(v[i+1,j]-2*v[i,j]+v[i-1,j])+(del_t/float(del_y)**2*(v[i,j+1]-2*v[i,j]+v[i,j-1])))

    # velocity boundary conditions
    u_new[0,:] = u_0
    u_new[-1,:] = u_new[-2,:]
    u_new[:,0] = 0
    u_new[:,-1] = 0

    v_new[0, :] = 0
    v_new[-1, :] = 0
    v_new[:, 0] = 0
    v_new[:, -1] = 0

    u_new[x_obs:(x_obs + wdth), y_obs:(y_obs + dpth)] = 0  # velocity in the obstacle is 0
    v_new[x_obs:(x_obs + wdth), y_obs:(y_obs + dpth)] = 0

    # Assigning old new values to the old variables
    for i in range(x_points):
        for j in range(y_points):
            u[i, j] = u_new[i, j]
            v[i, j] = v_new[i, j]
            P[i, j] = P_new[i, j]

    if it%100 == 0:
        print("Completed iteration: " + str(it))

print ("\n Pressure distribution is: \n" + str(P))

# Plotting results
plt.contourf(y_gr, x_gr, P, cmap = 'cool', alpha = 0.5)
plt.colorbar()
plt.quiver(y_gr, x_gr, u, v)        # Plot u and v as arrows
# https://pythontic.com/visualization/charts/brokenbarchart_horizontal - Plotting the obstacle
yrange = [(y_gr[y_obs], dpth*del_y)]
xrange = (x_gr[x_obs], wdth*del_x)
plt.broken_barh(yrange, xrange, facecolors = 'red', alpha = 0.8)
plt.title("Pressure distribution at time-step :" + str(num_itrs))
plt.show()
