# A property is transported by means of convection and diffusion through the one-dimensional domain.
# The boundary conditions are phi_0 =1 at x=0 and phi_L =0 at x=L.
# Calculate the distribution of phi as a function of x. Length L = 1.0 m, density = 1.0 kg/m3, Gamma=0.1 kg/ms

import numpy as np
from matplotlib import pyplot as plt


# Define function to calculate diffusive conductance
def D(dist):
    return Gamma/float(dist)


# Define function to calculate flow rate
def F(vel):
    return rho*vel


# Define parameters
rho = 1.0
Gamma = 0.1
u = 2.5     # Change Speed to 0.1 m/s also

# Grid generation
x_len = 1
x_points = 100    # Excluding the boundary points
del_x = x_len/float(x_points+1)
x = np.arange(x_points+1)
phi = np.arange(x_points+1)
aW = np.zeros(x_points+1)
aE = np.zeros(x_points+1)
aP = np.zeros(x_points+1)

Coeff_mat = np.zeros((x_points, x_points))
Sol_mat = np.zeros(x_points)

# Boundary Conditions
phi_0 = 1
phi_L = 0

# For boundary node 1
aW[0] = 0
aE[0] = D(del_x) - (F(u)/float(2.0))
Sp = -1.0 * ((2.0 * D(del_x)) + F(u))
Su1 = phi_0 * ((2.0 * D(del_x)) + F(u))
aP[0] = aW[0] + aE[0] - Sp

# For boundary node -1
aE[-1] = 0
aW[-1] = D(del_x) + (F(u)/float(2.0))
Sp = -1.0 * ((2.0 * D(del_x)) - F(u))
Su2 = phi_L * ((2.0 * D(del_x)) - F(u))
aP[-1] = aW[-1] + aE[-1] - Sp


# Step 3: Solution
for i in range(1, x_points-1):
    aW[i] = D(del_x) + (F(u)/float(2.0))
    aE[i] = D(del_x) - (F(u)/float(2.0))
    Sp = 0
    Su3 = 0
    aP[i] = aW[i] + aE[i] - Sp

    for j in range(1, x_points-1):
        Coeff_mat[i,i] = aP[i]
        Coeff_mat[i,i+1] = -1*aE[i]
        Coeff_mat[i,i-1] = -1*aW[i]

    Sol_mat[i] = Su3

Coeff_mat[0,0] = aP[0]
Coeff_mat[0,1] = -1*aE[0]
Coeff_mat[-1,-1] = aP[-1]
Coeff_mat[-1,-2] = -1*aW[-1]
Sol_mat[0] = Su1
Sol_mat[-1] = Su2

print("\n Coefficient matrix is: " +str(Coeff_mat))
print("\n Solution matrix is: " + str(Sol_mat))

phi = np.linalg.solve(Coeff_mat,Sol_mat)

phi[0] = phi_0
phi[-1] = phi_L
print ("Flux distribution is: " + str(phi))

# Plotting the result
plt.plot(phi)
plt.show()
