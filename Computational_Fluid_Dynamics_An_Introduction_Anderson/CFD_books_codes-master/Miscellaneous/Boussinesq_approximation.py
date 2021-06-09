# http://ohllab.org/CFD_course/Minh%2520FDM%2520Boussinesq.html

import math
import numpy as np
from matplotlib import pyplot as plt

# Define the domain
x_len = 1
y_len = 1
x_points = 41
y_points = 41
del_x = x_len/float(x_points-1)
del_y = y_len/float(y_points-1)
x_gr = np.linspace(0, x_len, x_points)
y_gr = np.linspace(0, y_len, y_points)

# Define the parameters
nu = 0.1
rho = 1.0
T_0 = 0
T_L = 1
D = 0.1
g = 1
beta = 0.01
del_t = 0.001
num_itrs = 200

# Define the variable
u = np.zeros((x_points, y_points))
v = np.zeros((x_points, y_points))
for i in range(1,x_points):
    v[i,0] = math.sin(4* np.pi/float(10*x_gr[i]))
P = np.zeros((x_points, y_points))
T = np.ones((x_points, y_points))*T_0
T[-1, :] = T_L

u_new = np.zeros((x_points, y_points))
v_new = np.zeros((x_points, y_points))
for i in range(1,x_points):
    v_new[i,0] = math.sin(4* np.pi/float(10*x_gr[i]))
P_new = np.zeros((x_points, y_points))
T_new = np.ones((x_points, y_points))*T_0
T[-1, :] = T_L

# Iteration
for itr in range(num_itrs):
    for i in range(1, x_points-1):
        for j in range(1, y_points - 1):
            P_new[i, j] = ((P[i + 1, j] + P[i - 1, j]) * del_x * del_x + (P[i, j + 1] + P[i, j - 1]) * del_y * del_y) / float(2 * (del_x * del_x + del_y * del_y)) - (del_x * del_x * del_y * del_y / float(2 * (del_x * del_x + del_y * del_y))) * ( (rho/float(del_t))*(((u[i+1,j] - u[i-1,j])/float(2*del_x)) + ((v[i,j+1] - v[i,j-1])/float(2*del_y))) - rho*((( (u[i+1,j] - u[i-1,j])/float(2*del_x) ) * ( (u[i+1,j] - u[i-1,j])/float(2*del_x) )) + (2* ((u[i+1,j] - u[i-1,j])/float(2*del_y)) * ((v[i,j+1] - v[i,j-1])/float(2*del_x))) + (((v[i,j+1] - v[i,j-1])/float(2*del_y)) * ((v[i,j+1] - v[i,j-1])/float(2*del_y)))) )

    # Boundary conditions for P
    P_new[0, 0] = 0
    P_new[0, :] = P_new[1, :]
    P_new[-1, :] = P_new[-2, :]
    P_new[:, 0] = P_new[:, 1]
    P_new[:, -1] = P_new[:, -2]

    for i in range(1, x_points-1):
        for j in range(1, y_points - 1):
            u_new[i,j] = u[i,j]-u[i,j]*(del_t/float(del_x))*(u[i+1,j]-u[i-1,j])-v[i,j]*(del_t/float(del_y))*(u[i+1,j]-u[i-1,j])-del_t/(float(2*rho*del_x))*(P_new[i+1,j]-P_new[i-1,j])+nu*((del_t/float(del_x))**2*(u[i+1,j]-2*u[i,j]+u[i-1,j])+((del_t/float(del_y))**2*(u[i+1,j]-2*u[i,j]+u[i-1,j])))
            v_new[i,j] = v[i,j]-u[i,j]*(del_t/float(del_x))*(v[i,j+1]-v[i,j-1])-v[i,j]*(del_t/float(del_y))*(v[i,j+1]-  v[i,j-1])-del_t/(float(2*rho*del_y))*(P_new[i,j+1]-P_new[i,j-1])+nu*((del_t/float(del_x))**2*(v[i,j+1]-2*v[i,j]+v[i,j-1])+((del_t/float(del_y))**2*(v[i,j+1]-2*v[i,j]+v[i,j-1])))-(1-beta*(T[i,j]-T_0))*g*del_t

            T_new[i,j] = T[i,j]+D*del_t*((T[i+1,j]-2*T[i,j]+T[i-1,j])/(float(del_x**2))+(T[i,j+1]-2*T[i,j]+T[i,j-1])/(float(del_y**2)))-(u[i,j]*(T[i+1,j]-T[i-1,j])/(float(2*del_x))+v[i,j]*(T[i,j+1]-T[i,j-1])/(float(2*del_y)))*del_t-T[i,j]*((u[i+1,j]-u[i-1,j])/(float(2*del_x))+T[i,j]*(v[i,j+1]-v[i,j-1]))/(float(2*del_y))*del_t

    # Boundary conditions for u and v
    u_new[0, :] = 0
    u_new[-1, :] = 0
    u_new[:, 0] = 0
    u_new[:, -1] = 0

    for i in range(1, x_points - 1):
        v_new[i, 0] = 0.01*itr*math.sin(4* np.pi/float(10*x_gr[i]))*np.exp(-0.01*itr)
    v_new[-1, :] = 0
    v_new[0, :] = 0
    v_new[:, -1] = 0

    # Boundary conditions for T
    T_new[0, :] = T_0
    T_new[-1, :] = T_L
    T_new[:, 0] = 2*T_new[:, 1] - T_new[:, 2]
    T_new[:, -1] = 2*T_new[:, -2] - T_new[:, -3]

    # Assign the new values to previous ones
    for i in range(x_points):
        for j in range(y_points):
            P[i,j] = P_new[i,j]
            u[i, j] = u_new[i, j]
            v[i, j] = v_new[i, j]
            T[i, j] = T_new[i, j]

print ("\n Pressure is: \n" + str(P))
print ("\n u velocity is: \n" + str(u))
print ("\n v velocity is: \n" + str(v))
print ("\n Temperature is: \n" + str(T))

# Plot temperature
plt.contourf(x_gr, y_gr, T)
plt.colorbar()
plt.quiver(x_gr, y_gr, u, v, 0)
plt.title("Temperature distribution with sinusoidal excitation")
plt.show()
