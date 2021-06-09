import numpy as np
from matplotlib import pyplot as plt

# Define the domain
x_len = 10
y_len = 10
x_points = 21
y_points = 21
n_points = x_points*y_points
del_x = x_len/float(x_points - 1)
del_y = x_len/float(y_points - 1)
Sc = 50      # Linearization of source term
Sp = 0
K = 1        # Assuming equal K throughout the domain

x = np.arange(x_points+1)
y = np.arange(y_points+1)
f = 0.5     # Assuming equal fraction in the control volume shared between two adjacent grids
x_w = np.arange(x[1] - f, x[-2], 1)
x_e = np.arange(x[1] + f, x[-1], 1)
y_s = np.arange(y[1] - f, y[-2], 1)
y_n = np.arange(y[1] + f, y[-1], 1)

# Temp = np.zeros(n_points)
Temp = np.zeros((x_points, y_points))

T1 = 100    # Temperature value at all boundaries specified as constant

# Discretization equation
num_itrs = 1000     # Number of iterations for Gauss-Siedel method

Temp[:,0] = T1
Temp[0,:] = T1
Temp[:,-1] = T1
Temp[-1,:] = T1

for nitr in range(num_itrs):
    for i in range(1, x_points-1):
        for j in range(1, y_points-1):
            del_x_e = x[i + 1] - x[i]
            del_x_w = x[i] - x[i - 1]
            del_y_s = y[j] - y[j - 1]
            del_y_n = y[j + 1] - y[j]
            ke = 1 / float(((x_e[i - 1] - x[i]) / float(K)) + ((x[i + 1] - x_e[i - 1]) / float(K)))
            kw = 1 / float(((x[i] - x_w[i - 1]) / float(K)) + ((x_w[i - 1] - x[i - 1]) / float(K)))
            kn = 1 / float(((y_n[j - 1] - y[j]) / float(K)) + ((y[j + 1] - y_n[j - 1]) / float(K)))
            ks = 1 / float(((y[j] - y_s[j - 1]) / float(K)) + ((y_s[j - 1] - y[j - 1]) / float(K)))

            aE = K/float(del_x_e)
            aW = K/float(del_x_w)
            aN = K/float(del_y_n)
            aS = K/float(del_y_s)
            aP = aE + aW + aN + aS + (Sp * del_x * del_y)
            b = Sc * del_x * del_y

            Temp[i,j] = (aW * Temp[i - 1,j] + aE * Temp[i + 1,j] + aN * Temp[i, j + 1] + aS * Temp[i, j - 1] + b) / float(aP)

print ("\n Temperature distribution is: \n" + str(Temp))
print ("\n The max temperature is: \t" + str(Temp.max()))

plt.pcolormesh(x,y,Temp)
plt.colorbar()
plt.show()
