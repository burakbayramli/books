# http://cav2012.sg/cdohl/CFD_course/VOF%252002%2520Advection%2520of%2520Density.html

import numpy as np
from matplotlib import pyplot as plt

# Define the domain
x_len = 1.0
y_len = 1.0
x_points = 21
y_points = 21
del_x = x_len/float(x_points-1)
del_y = y_len/float(y_points-1)

# Define parameters
num_itrs = 3000
del_t = 0.0001
rho_0 = 1.0
gx = 0.0
gy = 0.0
u_lid = 2.0
nu = 0.05
rho_corner = 1000
beta = 1.0

u = np.zeros((x_points+1, y_points+2))
u[:,-1] = u_lid
v = np.zeros((x_points+2, y_points+1))
P = np.zeros((x_points+2, y_points+2))
rho = np.ones((x_points+2, y_points+2))*rho_0

u_new = np.zeros((x_points+1, y_points+2))
u_new[:,-1] = u_lid
v_new = np.zeros((x_points+2, y_points+1))
P_new = np.zeros((x_points+2, y_points+2))
rho_new = np.ones((x_points+2, y_points+2))*rho_0
rho_press = np.ones((x_points+2, y_points+2))*rho_0
rho_press[0, :] = rho_corner
rho_press[-1, :] = rho_corner
rho_press[:, -1] = rho_corner
rho_press[:, 0] = rho_corner

u_avg = np.zeros((x_points, y_points))
v_avg = np.zeros((x_points, y_points))
P_avg = np.zeros((x_points, y_points))
rho_avg = np.zeros((x_points, y_points))

# Iteration
for its in range(num_itrs):
    for i in range(1, x_points):
        for j in range(1, y_points+1):
            u_new[i,j] = u[i,j]+del_t*(-0.25*(((u[i+1,j]+u[i,j])**2-(u[i,j]+u[i-1,j])**2)/float(del_x)+((u[i,j+1]+u[i,j])*(v[i+1,j]+v[i,j])-(u[i,j]+u[i,j-1])*(v[i+1,j-1]+v[i,j-1]))/float(del_y))+nu/(0.5*(rho[i+1,j]+rho[i,j]))*((u[i+1,j]-2*u[i,j]+u[i-1,j])/float(del_x)**2+(u[i,j+1]-2*u[i,j]+u[i,j-1])/float(del_y)**2 )+gx)

    for i in range(1, x_points+1):
        for j in range(1, y_points):
            v_new[i,j] = v[i,j]+del_t*(-0.25*(((u[i,j+1]+u[i,j])*(v[i+1,j]+v[i,j])-(u[i-1,j+1]+u[i-1,j])*(v[i,j]+v[i-1,j]))/float(del_x)+((v[i,j+1]+v[i,j])**2-(v[i,j]+v[i,j-1])**2)/float(del_y))+nu/(0.5*(rho[i,j+1]+rho[i,j]))*((v[i+1,j]-2*v[i,j]+v[i-1,j])/float(del_x)**2+(v[i,j+1]-2*v[i,j]+v[i,j-1])/float(del_y)**2 )+gy)

    # Boundary condition for velocity
    u_new[:, 0] = - u_new[:, 1]
    u_new[:,-1] = 2.0* u_lid - u_new[:, -2]
    v_new[0, :] = - v_new[1, :]
    v_new[-1, :] = - v_new[-2, :]

    for i in range(1, x_points+1):
        for j in range(1, y_points+1):
                P_new[i,j] = (1.0-beta)*P[i,j]+beta*(1.0/( (1./float(del_x))*( 1./(float(del_x)*(rho_press[i+1,j]+rho_press[i,j]))+1./(float(del_x)*(rho_press[i-1,j]+rho_press[i,j])))+(1./float(del_y))*(1./(float(del_y)*(rho_press[i,j+1]+rho_press[i,j]))+1./(float(del_y)*(rho_press[i,j-1]+rho_press[i,j])))))*((1./float(del_x))*( P[i+1,j]/(float(del_x)*(rho_press[i+1,j]+rho_press[i,j]))+P[i-1,j]/(float(del_x)*(rho_press[i-1,j]+rho_press[i,j])))+(1./float(del_y))*( P[i,j+1]/(float(del_y)*(rho_press[i,j+1]+rho_press[i,j]))+P[i,j-1]/(float(del_y)*(rho_press[i,j-1]+rho_press[i,j])))-((0.5/float(del_t))*( (u_new[i,j]-u_new[i-1,j])/float(del_x)+(v_new[i,j]-v_new[i,j-1])/float(del_y))))

    # Correcting velocities and updating to previous values
    for i in range(0, x_points):
        for j in range(0, y_points+1):
            u[i,j] = u_new[i,j]-del_t*(2.0/float(del_x))*(P[i+1,j]-P[i,j])/(rho[i+1,j]+rho[i,j])

    for i in range(0, x_points+1):
        for j in range(0, y_points):
            v[i,j] = v_new[i,j]-del_t*(2.0/float(del_y))*(P[i,j+1]-P[i,j])/(rho[i,j+1]+rho[i,j])

    # Density variation
    for i in range(1, x_points+1):
        for j in range(1, y_points+1):
            rho_new[i,j] = rho[i,j]-(0.5*del_t/float(del_x))*(u[i,j]*(rho[i+1,j]+rho[i,j])-u[i-1,j]*(rho[i-1,j]+rho[i,j]))-(0.5* del_t/float(del_y))*(v[i,j]*(rho[i,j+1]+rho[i,j])-v[i,j-1]*(rho[i,j-1]+rho[i,j]))+(nu*del_t/del_x/float(del_x))*(rho[i+1,j]-2.0*rho[i,j]+rho[i-1,j])+(nu*del_t/float(del_y)/float(del_y))*(rho[i,j+1]-2.0*rho[i,j]+rho[i,j-1])

    for i in range(0, x_points+2):
        for j in range(0, y_points+2):
                P[i,j] = P_new[i,j]

    for i in range(0, x_points+2):
        for j in range(0, y_points+2):
                rho[i,j] = rho_new[i,j]


print ("\n u velocity is : \n" + str(u))
print ("\n v velocity is : \n" + str(v))
print ("\n Pressure is : \n" + str(P))

for i in range(x_points):
    for j in range(y_points):
        u_avg[i, j] = (u[i, j] + u[i + 1, j]) / float(2)
        v_avg[i, j] = (v[i, j] + v[i, j + 1]) / float(2)
        rho_avg[i,j] = (rho[i, j] + rho[i+1, j] + rho[i, j+1]) / float(3)
        P_avg[i, j] = (P[i, j] + P[i + 1, j] + P[i, j + 1]) / float(3)

x_gr = np.linspace(0, x_len, x_points)
y_gr = np.linspace(0, y_len, y_points)
plt.streamplot(x_gr, y_gr, u_avg, v_avg, color="red")
plt.contourf(x_gr, y_gr, rho_avg)
plt.colorbar()
plt.title("Variation of density + Streamplot at iteration " + str(num_itrs))
plt.show()

plt.streamplot(x_gr, y_gr, u_avg, v_avg, color="red")
plt.contourf(x_gr, y_gr, P_avg)
plt.colorbar()
plt.title("Variation of pressure + Streamplot at iteration " + str(num_itrs))
plt.show()
