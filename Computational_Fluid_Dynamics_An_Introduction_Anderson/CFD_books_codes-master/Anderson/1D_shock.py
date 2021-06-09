# Shock wave code

import numpy as np
from matplotlib import pyplot as plt

# Define the domain
x_len = 3.0
x_points = 61
del_x = x_len / float(x_points - 1)
x_gr = np.linspace(0, x_len, x_points)

# Define the parameters
gamma = 1.4
A = 1 + 2.2 * (x_gr - 1.5) * (x_gr - 1.5)
A0 = min(A)
A = A / float(A0)
rho = np.zeros(x_points)
T = np.zeros(x_points)
V = np.zeros(x_points)

for i in range(x_points):
    if x_gr[i] < 1.5:
        rho[i] = 1 - 0.3146 * (x_gr[i] / float(x_len))
        T[i] = 1 - 0.2314 * (x_gr[i] / float(x_len))
        V[i] = 0.59 / float(rho[i] * A[i])
    elif x_gr[i] >= 1.5 and x_gr[i] < 2.1:
        rho[i] = 0.634 - 0.702 * ((x_gr[i] - 1.5) / float(x_len))
        T[i] = 0.833 - 0.4908 * ((x_gr[i] - 1.5) / float(x_len))
        V[i] = 0.59 / float(rho[i] * A[i])
    else:
        rho[i] = 0.5892 + 0.10228 * ((x_gr[i] - 2.1) / float(x_len))
        T[i] = 0.93968 + 0.0622 * ((x_gr[i] - 2.1) / float(x_len))
        V[i] = 0.59 / float(rho[i] * A[i])

P = rho * T
P[-1] = 0.6784

C = 0.5  # Courant number
del_ti = np.zeros(len(x_gr))
for i in range(len(x_gr)):
    del_ti[i] = C * (del_x / float(pow(T[i], 0.5) + V[i]))
# del_t = min(del_ti)  # Time step
del_t = 0.001

# Start iteration
num_itrs = 200

U1 = rho * A
U2 = rho * A * V
U3 = rho * A * ((T / float(gamma - 1)) + gamma * V * V / float(2))

U1_new = rho * A
U2_new = rho * A * V
U3_new = rho * A * ((T / float(gamma - 1)) + gamma * V * V / float(2))

U1_t_i = np.zeros(x_points)
U2_t_i = np.zeros(x_points)
U3_t_i = np.zeros(x_points)

U1_t_i_old = np.zeros(x_points)
U2_t_i_old = np.zeros(x_points)
U3_t_i_old = np.zeros(x_points)

F1 = rho * A
F2 = rho * A * V * V + (1 / float(gamma)) * P * A
F3 = rho * A * V * ((T / float(gamma - 1)) + ((gamma * V * V) / float(2))) + P * A * V

F1_new = rho * A
F2_new = rho * A * V * V + (1 / float(gamma)) * P * A
F3_new = rho * A * V * ((T / float(gamma - 1)) + ((gamma * V * V) / float(2))) + P * A * V

for itr in range(num_itrs):
    for i in range(0, x_points):
        F1[i] = U2[i]
        F2[i] = (U2[i] * U2[i] / float(U1[i])) + (
                    ((gamma - 1) / float(gamma)) * (U3[i] - (gamma * U2[i] * U2[i] / float(2 * U1[i]))))
        F3[i] = (gamma * U2[i] * U3[i] / float(U1[i])) - (
                    gamma * (gamma - 1) * U2[i] * U2[i] * U2[i] / float(2 * U1[i] * U1[i]))

    for i in range(0, x_points - 1):
        U1_t_i_old[i] = (F1[i + 1] - F1[i]) / float(del_x)
        U2_t_i_old[i] = ((F2[i + 1] - F2[i]) / float(del_x)) + (
                    (1 / float(gamma)) * rho[i] * T[i] * ((A[i + 1] - A[i]) / float(del_x)))
        U3_t_i_old[i] = (F3[i + 1] - F3[i]) / float(del_x)

    for i in range(0, x_points - 1):
        U1_new[i] = U1[i] + (U1_t_i_old[i]) * del_t
        U2_new[i] = U2[i] + (U2_t_i_old[i]) * del_t
        U3_new[i] = U3[i] + (U3_t_i_old[i]) * del_t

    # Boundary condition
    # rho_new[0] = 2*rho[1] - rho[2]  # flow-field variables are calculated by linear extrapolation
    # T_new[0] = 2*T[1] - T[2]
    # U1_new[0] = A[0]
    # U2_new[0] = 2 * U2_new[1] - U2_new[2]
    # U3_new[0] = U1_new[0] * (T[0] / float(gamma - 1) + (gamma * V[0] * V[0]) / float(2))
    U1_new[0] = 2*U1_new[1] - U1_new[2]
    U2_new[0] = 2*U2_new[1] - U2_new[2]
    U3_new[0] = 2*U3_new[1] - U3_new[2]

    U1_new[-1] = 2 * U1_new[-2] - U1_new[-3]  # flow-field variables are calculated by linear extrapolation
    U2_new[-1] = 2 * U2_new[-2] - U2_new[-3]
    # U3_new[-1] = 2 * U3_new[-2] - U3_new[-3]
    U3_new[-1] = P[-1]*A[-1]/float(gamma-1) + (gamma/float(2))*(U2_new[-1]*U2_new[-1]/float(U1_new[-1]))

    # Updating rho and T
    for i in range(0, x_points):
        rho[i] = U1_new[i]/float(A[i])
        T[i] = (gamma - 1) * ((U3_new[i] / float(U1_new[i])) - (gamma / float(2)) * (U2_new[i] / float(U1_new[i])) * (
                    U2_new[i] / float(U1_new[i])))

    # Corrector step
    for i in range(0, x_points):
        F1_new[i] = U2_new[i]
        F2_new[i] = (U2_new[i] * U2_new[i] / float(U1_new[i])) + (
                    ((gamma - 1) / float(gamma)) * (U3_new[i] - (gamma * U2_new[i] * U2_new[i] / float(2 * U1_new[i]))))
        F3_new[i] = (gamma * U2_new[i] * U3_new[i] / float(U1_new[i])) - (
                    gamma * (gamma - 1) * U2_new[i] * U2_new[i] * U2_new[i] / float(2 * U1_new[i] * U1_new[i]))

    for i in range(1, x_points):
        U1_t_i[i] = (F1_new[i] - F1_new[i - 1]) / float(del_x)
        U2_t_i[i] = ((F2_new[i] - F2_new[i - 1]) / float(del_x)) + (
                    (1 / float(gamma)) * rho[i] * T[i] * ((A[i] - A[i - 1]) / float(del_x)))
        U3_t_i[i] = (F3_new[i] - F3_new[i - 1]) / float(del_x)

    for i in range(1, x_points-1):
        U1_new[i] = U1[i] + 0.5 * (U1_t_i[i] + U1_t_i_old[i]) * del_t
        U2_new[i] = U2[i] + 0.5 * (U2_t_i[i] + U2_t_i_old[i]) * del_t
        U3_new[i] = U3[i] + 0.5 * (U3_t_i[i] + U3_t_i_old[i]) * del_t

    # Assigning the current values to previous array
    for i in range(x_points):
        U1[i] = U1_new[i]
        U2[i] = U2_new[i]
        U3[i] = U3_new[i]

    for i in range(x_points):
        rho[i] = U1[i]
        V[i] = U2[i] / float(U1[i])
        T[i] = (gamma - 1) * (U3[i] / float(U1[i]) - (gamma * V[i] * V[i]) / float(2))

print ("Density variation" + str(rho))
print ("Temperature variation" + str(T))
print ("Velocity variation" + str(V))

# print ("U1 variation" + str(U1))
# print ("U2 variation" + str(U2))
# print ("U3 variation" + str(U3))

plt.plot(x_gr, A, label="Nozzle profile")
plt.legend()
plt.show()
plt.plot(x_gr, rho, label="Density")
plt.legend()
plt.show()
plt.plot(x_gr, T, label="Temperature")
plt.legend()
plt.show()
plt.plot(x_gr, V, label="Velocity")
plt.legend()
plt.show()
plt.plot(x_gr, rho * T, label="Pressure")
plt.legend()
plt.show()
plt.plot(x_gr, rho * V * A, label="Mass flow rate")
plt.legend()
plt.show()
