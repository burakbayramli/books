# Consider the problem of source-free heat conduction in an insulated rod whose ends are maintained at
# constant temperatures of 100 C and 500 C respectively. Calculate the steady state temperature distribution in the rod.
# Thermal conductivity k equals 1000 W/mK, cross-sectional area A is 10 x 10-3 m2.

import numpy as np
from matplotlib import pyplot as plt


# Define parameters
k = 1000
A = 10*0.001

# Grid generation
x_len = 0.5
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
T2 = 500

# For boundary 1
aW[0] = 0
aE[0] = k * A / float(del_x)
Sp = -2 * k * A / float(del_x)
Su1 = (-2 * k * A / float(del_x)) * T1
aP[0] = aW[0] + aE[0] - Sp

# For boundary -1
aE[-1] = 0
aW[-1] = k * A / float(del_x)
Sp = -2 * k * A / float(del_x)
Su2 = (-2 * k * A / float(del_x)) * T2
aP[-1] = aW[-1] + aE[-1] - Sp


# Step 3: Solution
for i in range(1, x_points-1):
    aW[i] = k * A / float(del_x)
    aE[i] = k * A / float(del_x)
    aP[i] = aW[i] + aE[i]

    for j in range(1, x_points-1):
        Coeff_mat[i,i] = aP[i]
        Coeff_mat[i,i+1] = -1*aE[i]
        Coeff_mat[i,i-1] = -1*aW[i]

Coeff_mat[0,0] = aP[0]
Coeff_mat[0,1] = -1*aE[0]
Coeff_mat[-1,-1] = aP[-1]
Coeff_mat[-1,-2] = -1*aW[-1]
Sol_mat[0] = Su1
Sol_mat[-1] = Su2

print(Coeff_mat)
print(Sol_mat)

T = -1* np.linalg.solve(Coeff_mat,Sol_mat)
Temp = np.zeros(x_points + 1)
for i in range(1, x_points):
    Temp[i] = T[i]
Temp[0] = T1
Temp[-1] = T2
print (Temp)

# Plotting the result
plt.plot(Temp)
plt.show()
