# Consider solving a one-dimensional conduction equation for an insulated metal rod which has internal heat generation.
# The dimensions and other data are as follows- length of the rod is 1 m, cross-sectional area of the rod is 0.01 m2,
# Thermal conductivity k = 5 W/mK, generation g = 20 kW/m3, the ends are at 100 C and 500 C

import numpy as np
from matplotlib import pyplot as plt

# Define the domain
x_len = 1.0
x_points = 20
del_x = x_len/float(x_points-1)

x_gr = np.linspace(0, x_len, x_points, dtype=float)

# Define the parameters
k = 5.0
g = 20.0 * 1000
A = 0.01
T_0 = 100
T_L = 500

# Generate the equations
coeff_mat = np.zeros((x_points,x_points))
sol_mat = np.zeros(x_points)

for i in range(1, x_points-1):
    aW = k * A / float(del_x)
    aE = k * A / float(del_x)
    Sp = 0
    Su = g*A*del_x
    aP = aW + aE - Sp

    coeff_mat[i,i] = aP
    coeff_mat[i, i+1] = -1.0*aE
    coeff_mat[i,i-1] = -1.0*aW
    sol_mat[i] = Su

## Boundary 0
aW = 0
aE = k * A / float(del_x)
Sp = -1.0*k*A/float(del_x/float(2))
Su = g*A*del_x + T_0*k*A/float(del_x/float(2))
aP = aW + aE - Sp

coeff_mat[0,0] = aP
coeff_mat[0,1] = -1.0*aE
sol_mat[0] = Su

## Boundary -1
aE = 0
aW = k * A / float(del_x)
Sp = -1.0*k*A/float(del_x/float(2))
Su = g*A*del_x + T_L*k*A/float(del_x/float(2))
aP = aW + aE - Sp

coeff_mat[-1,-1] = aP
coeff_mat[-1,-2] = -1.0*aW
sol_mat[-1] = Su

# Solution using numpy (built-in function)
T_np = np.linalg.solve(coeff_mat,sol_mat)
print ("\n The solution vector using numpy is: \n" + str(T_np))


# Solution using TDMA (Tri diagonal matrix algorithm)
T_tdma = np.ones(x_points)
A = np.zeros(x_points)
C_dash = np.zeros(x_points)
A[0] = coeff_mat[0,1]/float(coeff_mat[0,0])
C_dash[0] = sol_mat[0]/float(coeff_mat[0,0])
for i in range(1, x_points-1):
    A[i] = -1.0 * coeff_mat[i, i + 1] / float(coeff_mat[i, i] - -1.0 * coeff_mat[i, i-1] * A[i - 1])
    C_dash[i] = (sol_mat[i] + -1.0 * coeff_mat[i, i-1] * C_dash[i - 1]) / float(coeff_mat[i, i] - -1.0 * coeff_mat[i, i-1] * A[i - 1])

C_dash[-1] = (sol_mat[-1] + -1.0 * coeff_mat[-1, -2] * C_dash[-2]) / float(coeff_mat[-1,-1] - -1.0 * coeff_mat[-1,-2] * A[-2])
A[-1] = 0
T_tdma[-1] = C_dash[-1]

for i in range(x_points-2, -1, -1):
    T_tdma[i] = A[i]*T_tdma[i+1] + C_dash[i]

print ("\n The solution vector using TDMA is: \n" + str(T_tdma))


# Solution using Gauss-Siedel iteration
T_guess = 100*np.random.rand(1)           # Starting guess temperature value - chosen random
print ("\n Guess temperature for iteration is : \t" + str(T_guess) + "\n")
T_gs = np.ones(x_points)*T_guess
num_itrs = 100                            # Number of iterations in Gauss-Siedel method
for it in range(num_itrs):
    for i in range(0, x_points):
        sum = 0
        for j in range(0, x_points):
            if i != j:
                sum = sum + (coeff_mat[i, j] * T_gs[j])
        T_gs[i] = (sol_mat[i] - sum) / coeff_mat[i, i]

print ("\n The solution vector using Gauss Siedel is: \n" + str(T_gs))


# Plotting the solution vectors
plt.plot(x_gr, T_np, label = "numpy")
plt.plot(x_gr, T_tdma, label = "TDMA")
plt.plot(x_gr, T_gs, label = "Gauss-Siedel")
plt.xlabel("Length")
plt.ylabel("Temperature")
plt.legend()
plt.show()
