# A cylindrical fin with uniform cross-sectional area A.The base is at a temperature of 100 C and the end is insulated.
# The fin is exposed to an ambient temperature of 20 C. L = 1 m, hP/kA = 25/m2 (note that kA is constant).

import numpy as np
from matplotlib import pyplot as plt


# Define parameters
k = 1000
n = 25      # n = hP/kA

# Grid generation
x_len = 1
x_points = 100    # Excluding the boundary points
del_x = x_len/float(x_points+1)
x = np.arange(x_points+1)
T = np.arange(x_points+1)
aW = np.zeros(x_points+1)
aE = np.zeros(x_points+1)
aP = np.zeros(x_points+1)

Coeff_mat = np.zeros((x_points, x_points))
Sol_mat = np.zeros(x_points)

# Boundary Conditions
T1 = 100
T2 = 20

# For boundary 1
aW[0] = 0
aE[0] = 1.0 / float(del_x)
Sp = (-1.0 * n * del_x) - (2.0 / float(del_x))
Su1 = (n * del_x * T2) + (2.0 * T1/ float(del_x))
aP[0] = aW[0] + aE[0] - Sp

# For boundary -1
aE[-1] = 0
aW[-1] = 1.0 / float(del_x)
Sp = -1.0 * n * del_x
Su2 = n * del_x * T2
aP[-1] = aW[-1] + aE[-1] - Sp


# Step 3: Solution
for i in range(1, x_points-1):
    aW[i] = 1.0 / float(del_x)
    aE[i] = 1.0 / float(del_x)
    Sp = -1.0 * n * del_x
    Su3 = n * del_x * T2
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

T = np.linalg.solve(Coeff_mat,Sol_mat)
Temp = np.zeros(x_points + 1)
for i in range(1, x_points):
    Temp[i] = T[i]
Temp[0] = T1
Temp[-1] = T2
print ("Temperature distribution is: " + str(Temp))

# Plotting the result
plt.plot(x, Temp)
plt.show()
