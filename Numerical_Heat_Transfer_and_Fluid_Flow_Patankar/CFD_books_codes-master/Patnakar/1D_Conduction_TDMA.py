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

P = np.zeros(n_points)
Q = np.zeros(n_points)
Temp = np.zeros(n_points)

T1 = 100    # Temperature value at x = 0 boundary specified
a, b, c, d = 1, 0, 0, T1
P[0] = b/float(a)
Q[0] = d/float(a)


# Discretization equation
for i in range(1, n_points):
    P[i] = b/float(a - (c*P[i-1]))
    Q[i] = (d + (c*Q[i-1]))/float(a - (c*P[i-1]))

    del_x_e = x[i+1] - x[i]
    del_x_w = x[i] - x[i-1]
    ke = 1 / float(((x_e[i - 1] - x[i]) / float(K[i])) + ((x[i + 1] - x_e[i - 1]) / float(K[i+1])))
    kw = 1 / float(((x[i] - x_w[i - 1]) / float(K[i-1])) + ((x_w[i - 1] - x[i - 1]) / float(K[i])))

    aE = ke / float(del_x_e)
    aW = kw / float(del_x_w)
    aP = aE + aW + (Sp*del_x)

    b = aE
    c = aW
    d = Sc*del_x
    a = aP

Temp[-1] = Q[-1]
P[-1] = 0

for i in range(n_points-2, 0, -1):
    Temp[i] = P[i]*Temp[i+1] + Q[i]     # TDMA algorithm

Temp[0] = T1
print ("Temperature distribution is: \n" + str(Temp))

plt.plot(Temp)
plt.show()
