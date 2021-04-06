import numpy as np
import math
from matplotlib import pyplot as plt

# Define the domain
x_len = 65
y_len = 40
x_points = 41
y_points = 41
del_x = x_len / float(x_points - 1)
del_y = y_len / float(x_points - 1)
xE = 10
x = np.linspace(0, x_len, x_points)
y = np.linspace(0, y_len, y_points)
theta = math.radians(5.352)
h = np.ones(x_points)
ys = np.ones(x_points)
zeta = x
eta = np.ones(x_points)
deleta_delx = np.ones(x_points)
for i in range(x_points):
    if x[i] < xE:
        h[i] = 40
        ys[i] = 0
        eta[i] = (y[i] - ys[i]) / float(h[i])
        deleta_delx[i] = 0
    else:
        h[i] = 40 + (x[i] - 10) * math.tan(theta)
        ys[i] = -(x[i] - xE) * math.tan(theta)
        eta[i] = (y[i] - ys[i]) / float(h[i])
        deleta_delx[i] = (1 - eta[i]) * math.tan(theta) / float(h[i])

del_zeta = np.absolute(zeta[1] - zeta[0])
del_eta = np.absolute(eta[1] - eta[0])

# Define parameters
gamma = 1.4
M = np.ones((x_points, y_points)) * 2
P = np.ones((x_points, y_points)) * 1.01 * pow(10, 5)
rho = np.ones((x_points, y_points)) * 1.23
T = np.ones((x_points, y_points)) * 286
u = M * pow(1.4 * 287 * T, 0.5)
v = np.zeros((x_points, y_points))

F1 = rho*u
G1 = rho*v
F2 = rho*u*u + P
G2 = rho*u*v
F3 = rho*u*v
G3 = rho*v*v + P
F4 = (gamma/float(gamma-1))*P*u + ((u*u + v*v)/float(2))*rho*u
G4 = (gamma/float(gamma-1))*P*v + ((u*u + v*v)/float(2))*rho*v

delF1_delzeta = np.zeros((x_points, y_points))
delF2_delzeta = np.zeros((x_points, y_points))
delF3_delzeta = np.zeros((x_points, y_points))
delF4_delzeta = np.zeros((x_points, y_points))

num_itrs = 2000
for itr in range(num_itrs):
    for i in range(0, x_points):
        for j in range(0, y_points - 1):
            delF1_delzeta[i, j] = (deleta_delx[i]) * ((F1[i, j] - F1[i, j + 1]) / float(del_eta)) + (
                        1 / float(h[i])) * ((G1[i, j] - G1[i, j + 1]) / float(del_eta))
            delF2_delzeta[i, j] = (deleta_delx[i]) * ((F2[i, j] - F2[i, j + 1]) / float(del_eta)) + (
                        1 / float(h[i])) * ((G2[i, j] - G2[i, j + 1]) / float(del_eta))
            delF3_delzeta[i, j] = (deleta_delx[i]) * ((F3[i, j] - F3[i, j + 1]) / float(del_eta)) + (
                        1 / float(h[i])) * ((G3[i, j] - G3[i, j + 1]) / float(del_eta))
            delF4_delzeta[i, j] = (deleta_delx[i]) * ((F4[i, j] - F4[i, j + 1]) / float(del_eta)) + (
                        1 / float(h[i])) * ((G4[i, j] - G4[i, j + 1]) / float(del_eta))

    for i in range(0, x_points - 1):
        for j in range(0, y_points):
            F1[i + 1, j] = F1[i, j] + delF1_delzeta[i, j] * del_zeta
            F2[i + 1, j] = F2[i, j] + delF2_delzeta[i, j] * del_zeta
            F3[i + 1, j] = F3[i, j] + delF3_delzeta[i, j] * del_zeta
            F4[i + 1, j] = F4[i, j] + delF4_delzeta[i, j] * del_zeta

    for i in range(0, x_points-1):
        for j in range(0, y_points):
            coeffA = (F3[i + 1, j] * F3[i + 1, j] / float(2 * F1[i + 1, j])) - F4[i + 1, j]
            coeffB = gamma * F1[i + 1, j] * F2[i + 1, j] / float(gamma - 1)
            coeffC = -1.0*(gamma + 1) * F1[i + 1, j] * F1[i + 1, j] * F1[i + 1, j] / float(2 * (gamma - 1))
            rho[i + 1, j] = (-1.0*coeffB + pow((coeffB * coeffB) - 4 * coeffA * coeffC, 0.5)) / float(2 * coeffA)

    for i in range(0, x_points - 1):
        for j in range(0, y_points):
            G1[i + 1, j] = rho[i + 1, j] * F3[i + 1, j] / float(F1[i + 1, j])
            G2[i + 1, j] = F3[i + 1, j]
            G3[i + 1, j] = (rho[i + 1, j] * F3[i + 1, j] * F3[i + 1, j] / float(F1[i + 1, j] * F1[i + 1, j])) + (
                        F2[i + 1, j]) - (F1[i + 1, j] * F1[i + 1, j] / float(rho[i + 1, j]))
            G4[i + 1, j] = ((gamma / float(gamma - 1)) * (
                        F2[i + 1, j] - (F1[i + 1, j] * F1[i + 1, j] / float(rho[i + 1, j]))) * (
                                        F3[i + 1, j] / float(F1[i + 1, j]))) + (
                                       (rho[i + 1, j] / float(2)) * (F3[i + 1, j] / float(F1[i + 1, j])) * (
                                           (F1[i + 1, j] * F1[i + 1, j] / float(rho[i + 1, j] * rho[i + 1, j])) + (
                                               F3[i + 1, j] * F3[i + 1, j] / float(F1[i + 1, j] * F1[i + 1, j]))))

    for i in range(0, x_points - 1):
        for j in range(1, y_points):
            delF1_delzeta[i + 1, j] = (deleta_delx[i]) * ((F1[i + 1, j - 1] - F1[i + 1, j]) / float(del_eta)) + (
                        1 / float(h[i])) * ((G1[i + 1, j - 1] - G1[i + 1, j]) / float(del_eta))
            delF2_delzeta[i + 1, j] = (deleta_delx[i]) * ((F2[i + 1, j - 1] - F2[i + 1, j]) / float(del_eta)) + (
                        1 / float(h[i])) * ((G2[i + 1, j - 1] - G2[i + 1, j]) / float(del_eta))
            delF3_delzeta[i + 1, j] = (deleta_delx[i]) * ((F3[i + 1, j - 1] - F3[i + 1, j]) / float(del_eta)) + (
                        1 / float(h[i])) * ((G3[i + 1, j - 1] - G3[i + 1, j]) / float(del_eta))
            delF4_delzeta[i + 1, j] = (deleta_delx[i]) * ((F4[i + 1, j - 1] - F4[i + 1, j]) / float(del_eta)) + (
                        1 / float(h[i])) * ((G4[i + 1, j - 1] - G4[i + 1, j]) / float(del_eta))

    for i in range(0, x_points - 1):
        for j in range(1, y_points):
            F1[i + 1, j] = F1[i, j] + 0.5 * (delF1_delzeta[i + 1, j] + delF1_delzeta[i, j]) * del_zeta
            F2[i + 1, j] = F2[i, j] + 0.5 * (delF2_delzeta[i + 1, j] + delF2_delzeta[i, j]) * del_zeta
            F3[i + 1, j] = F3[i, j] + 0.5 * (delF3_delzeta[i + 1, j] + delF3_delzeta[i, j]) * del_zeta
            F4[i + 1, j] = F4[i, j] + 0.5 * (delF4_delzeta[i + 1, j] + delF4_delzeta[i, j]) * del_zeta

    for i in range(0, x_points - 1):
        for j in range(0, y_points):
            coeffA = (F3[i + 1, j] * F3[i + 1, j] / float(2 * F1[i + 1, j])) - F4[i + 1, j]
            coeffB = gamma * F1[i + 1, j] * F2[i + 1, j] / float(gamma - 1)
            coeffC = -1.0 * (gamma + 1) * F1[i + 1, j] * F1[i + 1, j] * F1[i + 1, j] / float(2 * (gamma - 1))
            rho[i + 1, j] = (-1.0 * coeffB + pow((coeffB * coeffB) - 4 * coeffA * coeffC, 0.5)) / float(2 * coeffA)

            u[i + 1, j] = F1[i + 1, j] / float(rho[i + 1, j])
            v[i + 1, j] = F3[i + 1, j] / float(F1[i + 1, j])
            P[i + 1, j] = F2[i + 1, j] - (F1[i + 1, j] * u[i + 1, j])
            T[i + 1, j] = P[i + 1, j] / float(287 * rho[i + 1, j])

    # Boundary conditions
    for i in range(0, x_points-1):
        phi = math.atan(v[i, 0] / float(u[i, 0]))
        M_cal = pow((u[i, 0] * u[i, 0] + v[i, 0] * v[i, 0]) / float(gamma * 287 * T[i, 0]), 0.5)
        f_cal = pow((((gamma + 1) / float(gamma - 1)) * math.atan(
            pow(((gamma - 1) / float(gamma + 1)) * ((M_cal * M_cal) - 1), 0.5))) - math.atan(
            pow((M_cal * M_cal) - 1, 0.5)), 0.5)
        f_act = f_cal + phi
        # M_act = 1/float(math.sin(f_act))        # Completely wrong formula - trial and error method required
        # Calculate M from angle f - www.pdas.com/pm.pdf
        # nu = pow(f_act/float((math.sqrt(6)-1)*math.pi/float(2)), 2/float(3))
        # M_act = (1+ 1.3604*nu + 0.0962*nu*nu - 0.5127*nu*nu*nu)/float(1 - 0.6722*nu - 0.3278*nu*nu)
        M_act = 2
        P[i+1, 0] = P[i, 0] * pow(
            (1 + ((gamma - 1) / float(2)) * M_cal * M_cal) / float(1 + ((gamma - 1) / float(2)) * M_act * M_act),
            gamma / float(gamma - 1))
        T[i+1, 0] = T[i, 0] * pow(
            (1 + ((gamma - 1) / float(2)) * M_cal * M_cal) / float(1 + ((gamma - 1) / float(2)) * M_act * M_act),
            1)
        rho[i+1, 0] = P[i+1, 0] / float(287 * T[i+1,0])


    # Update BC
    for i in range(0, x_points-1):
        F1[i+1, 0] = rho[i+1, 0] * u[i+1, 0]
        G1[i+1, 0] = rho[i+1, 0] * v[i+1, 0]
        F2[i+1, 0] = rho[i+1, 0] * u[i+1, 0] * u[i+1, 0] + P[i+1, 0]
        G2[i+1, 0] = rho[i+1, 0] * u[i+1, 0] * v[i+1, 0]
        F3[i+1, 0] = rho[i+1, 0] * u[i+1, 0] * v[i+1, 0]
        G3[i+1, 0] = rho[i+1, 0] * v[i+1, 0] * v[i+1, 0] + P[i+1, 0]
        F4[i+1, 0] = (gamma / float(gamma - 1)) * P[i+1, 0] * u[i+1, 0] + (
                    (u[i+1, 0] * u[i+1, 0] + v[i+1, 0] * v[i+1, 0]) / float(2)) * rho[i+1, 0] * u[i+1, 0]
        G4[i+1, 0] = (gamma / float(gamma - 1)) * P[i+1, 0] * v[i+1, 0] + (
                    (u[i+1, 0] * u[i+1, 0] + v[i+1, 0] * v[i+1, 0]) / float(2)) * rho[i+1, 0] * v[i+1, 0]

print("\n Density is: \n" + str(rho))
print("\n Pressure is: \n" + str(P))
print("\n Temperature is: \n" + str(T))
print("\n u Velocity is: \n" + str(u))
print("\n v Velocity is: \n" + str(v))

plt.plot(x,h, label = "Profile")
plt.legend()
plt.show()
plt.contourf(x,y,rho)
plt.title("Density")
plt.colorbar()
plt.show()
plt.contourf(x,y,u)
plt.title("u velocity")
plt.colorbar()
plt.show()
plt.contourf(x,y,v)
plt.title("v velocity")
plt.colorbar()
plt.show()
plt.contourf(x,y,P)
plt.title("Pressure")
plt.colorbar()
plt.show()
plt.contourf(x,y,T)
plt.title("Temperature")
plt.colorbar()
plt.show()
