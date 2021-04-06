import numpy as np
from matplotlib import pyplot as plt

# Define the domain
x_len = 1
n_points = 50
del_x = x_len/float(n_points - 1)
Sc = 2      # Linearization of source term
Sp = 0
K = np.ones(n_points+1)

x = np.arange(n_points+1)
f = 0.5
x_w = np.arange(x[1] - f, x[-2],1)
x_e = np.arange(x[1] + f, x[-1],1)

Temp = np.zeros(n_points)

T1 = 100    # Temperature value at x = 0 boundary specified

# Discretization equation
num_itrs = 1000     # Number of iterations for Gauss-Siedel method
Temp[0] = T1
for nitr in range(num_itrs):
    for i in range(1, n_points-1):
        del_x_e = x[i + 1] - x[i]
        del_x_w = x[i] - x[i - 1]
        ke = 1 / float(((x_e[i - 1] - x[i]) / float(K[i])) + ((x[i + 1] - x_e[i - 1]) / float(K[i + 1])))
        kw = 1 / float(((x[i] - x_w[i - 1]) / float(K[i - 1])) + ((x_w[i - 1] - x[i - 1]) / float(K[i])))

        aE = ke / float(del_x_e)
        aW = kw / float(del_x_w)
        aP = aE + aW + (Sp * del_x)
        b = Sc * del_x

        Temp[i] = (aW * Temp[i - 1] + aE * Temp[i + 1] + b) / float(aP)

print ("Temperature distribution is: \n" + str(Temp))

plt.plot(Temp)
plt.show()
