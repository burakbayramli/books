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
u = 2.5     # Change Speed to 0.2 m/s also

# Grid generation
x_len = 1
x_points = 100    # Excluding the boundary points
del_x = x_len/float(x_points+1)
x = np.arange(x_points+1)
phi = np.arange(x_points+1)
aWW = np.zeros(x_points+1)
aW = np.zeros(x_points+1)
aE = np.zeros(x_points+1)
aP = np.zeros(x_points+1)

Coeff_mat = np.zeros((x_points, x_points))
Sol_mat = np.zeros(x_points)

# Boundary Conditions
phi_0 = 1
phi_L = 0

# ----------------------------------- # QUICK Algorithm # -----------------------------------

# For boundary node 1
aWW[0] = 0
aW[0] = 0
aE[0] = D(del_x/float(2)) - (3/float(8))*F(u) + (1/float(3))*D(del_x)
Sp = -1.0 * ( ((8/float(3)) * D(del_x)) + ((2/float(8)) * F(u)) + (F(u)) )
Su1 = phi_0 * ( ((8/float(3)) * D(del_x)) + ((2/float(8)) * F(u)) + (F(u)) )
aP[0] = aWW[0] + aW[0] + aE[0] - Sp

# For interior node 2
aWW[1] = 0
aW[1] = D(del_x/float(2)) + (6/float(8))*F(u) + (7/float(8))*F(u)
aE[1] = D(del_x/float(2)) - (3/float(8))*F(u)
Sp = (1.0/float(4)) * F(u)
Su4 = phi_0 * (1.0/float(4)) * F(u)
aP[1] = aWW[1] + aW[1] + aE[1] - Sp

# For boundary node -1
aE[-1] = 0
aWW[-1] = -1.0* (F(u)/ float(8))
aW[-1] = D(del_x/float(2)) + (6/float(8))*F(u) + (1/float(3))*D(del_x)
Sp = -1.0 * ( ((8/float(3)) * D(del_x)) - (F(u)) )
Su2 = phi_L * ( ((8/float(3)) * D(del_x)) - (F(u)) )
aP[-1] = aWW[-1] + aW[-1] + aE[-1] - Sp


# Step 3: Solution
for i in range(2, x_points-1):
    aWW[i] = (-1.0/float(8)) * F(u)
    aW[i] = D(del_x/float(2)) + (7/float(8))*F(u)
    aE[i] = D(del_x/float(2)) - (3/float(8))*F(u)
    Sp = 0
    Su3 = 0
    aP[i] = aWW[i] + aW[i] + aE[i] - Sp

    for j in range(2, x_points-1):
        Coeff_mat[i,i] = aP[i]
        Coeff_mat[i,i+1] = -1*aE[i]
        Coeff_mat[i,i-1] = -1*aW[i]
        Coeff_mat[i,i-2] = -1*aWW[i]

    Sol_mat[i] = Su3

Coeff_mat[0,0] = aP[0]
Coeff_mat[0,1] = -1*aE[0]

Coeff_mat[1,1] = aP[1]
Coeff_mat[1,0] = -1*aW[1]
Coeff_mat[1,2] = -1*aE[1]

Coeff_mat[-1,-1] = aP[-1]
Coeff_mat[-1,-2] = -1*aW[-1]
Coeff_mat[-1,-3] = -1*aWW[-1]

Sol_mat[0] = Su1
Sol_mat[1] = -1*Su4
Sol_mat[-1] = Su2

print("\n Coefficient matrix is: \n" +str(Coeff_mat))
print("\n Solution matrix is: \n" + str(Sol_mat))

phi = np.linalg.solve(Coeff_mat,Sol_mat)

phi[0] = phi_0
phi[-1] = phi_L
print ("\n Flux distribution is: \n" + str(phi))

# Plotting the result
plt.plot(phi)
plt.show()
