# Anderson 7.4.2

import numpy as np
from matplotlib import pyplot as plt

# Define the domain
x_len = 3.0
x_points = 31
del_x = x_len/float(x_points-1)
x_gr = np.linspace(0, x_len, x_points)


# Define the parameters
gamma = 1.4
A = np.zeros(x_points)
for i in range(0, x_points):
    if x_gr[i] < 1.5:
        A[i] = 1 + 2.2*(x_gr[i]-1.5)*(x_gr[i]-1.5)
    else:
        A[i] = 1 + 0.2223*(x_gr[i]-1.5)*(x_gr[i]-1.5)
A0 = min(A)
A = A/float(A0)
rho = 1 - 0.023*(x_gr/float(x_len))
T = 1 - 0.009333*(x_gr/float(x_len))
V = (0.05 + 0.11*x_gr/float(x_len))

rho_new = 1 - 0.023*(x_gr/float(x_len))
T_new = 1 - 0.009333*(x_gr/float(x_len))
V_new = (0.05 + 0.11*x_gr/float(x_len))

C = 0.5  # Courant number
del_ti = np.zeros(len(x_gr))
for i in range(len(x_gr)):
    del_ti[i] = C * (del_x / float(pow(T[i],0.5) + V[i]))
del_t = min(del_ti)
# del_t = 0.0001  # Time step

p_e = 0.93      # The exit pressure is specified

# Start iteration
num_itrs = 3500
rho_mid = np.zeros(num_itrs)

rho_t_i = np.zeros(x_points)
V_t_i = np.zeros(x_points)
T_t_i = np.zeros(x_points)

rho_t_i_old = np.zeros(x_points)
V_t_i_old = np.zeros(x_points)
T_t_i_old = np.zeros(x_points)

for itr in range(num_itrs):
    for i in range(1, x_points-1):
        rho_t_i_old[i] = -1.0*rho[i]*(V[i+1] - V[i])/float(del_x) - rho[i]*V[i]*(np.log(A[i+1]) - np.log(A[i]))/float(del_x) - V[i]*(rho[i+1] - rho[i])/float(del_x)
        V_t_i_old[i] = -1.0*V[i]*(V[i+1]-V[i])/float(del_x) - (1/float(gamma))*(((T[i+1] - T[i])/float(del_x)) + ((rho[i+1] - rho[i])*(T[i]/float(rho[i]*del_x))))
        T_t_i_old[i] = -1.0*V[i]*(T[i+1]-T[i])/float(del_x) - (gamma - 1)*T[i]*(((V[i+1] - V[i])/float(del_x)) + ((np.log(A[i+1]) - np.log(A[i]))*(V[i]/float(del_x))))
    
    for i in range(1, x_points-1):
        rho_new[i] = rho[i] + (rho_t_i_old[i])*del_t
        V_new[i] = V[i] + (V_t_i_old[i])*del_t
        T_new[i] = T[i] + (T_t_i_old[i])*del_t
    
    # Boundary condition
    # rho_new[0] = 2*rho[1] - rho[2]  # flow-field variables are calculated by linear extrapolation
    # T_new[0] = 2*T[1] - T[2]
    V_new[0] = 2*V[1] - V[2]
    
    rho_new[-1] = 2*rho[-2] - rho[-3]   # flow-field variables are calculated by linear extrapolation
    T_new[-1] = p_e/float(rho_new[-1])  # Temperature at exit is calculated from the specified pressure
    V_new[-1] = 2*V[-2] - V[-3]
    
    for i in range(1, x_points-1):
        rho_t_i[i] = -1.0*rho_new[i]*(V_new[i] - V_new[i-1])/float(del_x) - rho_new[i]*V_new[i]*(np.log(A[i]) - np.log(A[i-1]))/float(del_x) - V_new[i]*(rho_new[i] - rho_new[i-1])/float(del_x)
        V_t_i[i] = -1.0*V_new[i]*(V_new[i]-V_new[i-1])/float(del_x) - (1/float(gamma))*(((T_new[i] - T_new[i-1])/float(del_x)) + ((rho_new[i] - rho_new[i-1])*(T_new[i]/float(rho_new[i]*del_x))))
        T_t_i[i] = -1.0*V_new[i]*(T_new[i]-T_new[i-1])/float(del_x) - (gamma - 1)*T_new[i]*(((V_new[i] - V_new[i-1])/float(del_x)) + ((np.log(A[i]) - np.log(A[i-1]))*(V_new[i]/float(del_x))))
        
    for i in range(1, x_points-1):
        rho_new[i] = rho[i] + 0.5*(rho_t_i[i]+rho_t_i_old[i])*del_t
        V_new[i] = V[i] + 0.5*(V_t_i[i]+V_t_i_old[i])*del_t
        T_new[i] = T[i] + 0.5*(T_t_i[i]+T_t_i_old[i])*del_t
    
    # Assigning the curent values to previous array
    for i in range(x_points):
        rho[i] = rho_new[i]
        T[i] = T_new[i]
        V[i] = V_new[i]
    
    rho_mid[itr] = rho[16]

print ("Density variation" + str(rho))
print ("Temperature variation" + str(T))
print ("Velocity variation" + str(V))

plt.plot(x_gr, A, label = "Nozzle profile")
plt.legend()
plt.show()
plt.plot(x_gr, rho, label = "Density")
plt.legend()
plt.show()
plt.plot(x_gr, T, label = "Temperature")
plt.legend()
plt.show()
plt.plot(x_gr, V, label = "Velocity")
plt.legend()
plt.show()

plt.plot(x_gr, rho*A*V, label = "Mass flow rate")
plt.legend()
plt.show()
plt.plot(x_gr, rho*T, label = "Pressure")
plt.legend()
plt.show()
