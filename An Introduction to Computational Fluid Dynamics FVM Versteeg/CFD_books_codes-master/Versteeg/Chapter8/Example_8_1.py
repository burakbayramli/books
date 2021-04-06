# A thin plate is initially at a uniform temperature of 200 C. At a certain time t = 0 the temperature of the east side
# of the plate is suddenly reduced to 0 C. The other surface is insulated. Use the explicit finite volume method in
# conjunction with a suitable time step size to calculate the transient temperature distribution of the slab.
# Plate thickness L = 2 cm, thermal conductivity k = 10 W/mK and rho*c = 10 x 106 J/m3K

import numpy as np
from matplotlib import pyplot as plt

# Define the parameters
k = 10
rho_c = pow(10,7)
T_init = 200    # Initial temperature
T_L = 0         # At x= L, Boundary temperature

# Define the domain
x_len = 0.02
x_points = 7
del_x = x_len/float(x_points-1)
x_gr = np.linspace(0, x_len, x_points)        # Co-ordinates of grid points

del_t = 2           # Time step
del_t_max = rho_c*(del_x*del_x/float(2.0*k))        # Maximum time step allowed
print ("\n The max time step for stability using Explicit scheme is: \t" + str(del_t_max))

# Calculate the parameters at the nodes
Temp = np.zeros(x_points)
Temp_old = np.ones(x_points) * T_init
num_itrs = 20       # Number of iterations

for itrs in range(num_itrs):
    for i in range(1, x_points-1):
        aW = k / float(del_x)
        aE = k / float(del_x)
        Su = 0
        aP = rho_c * del_x / float(del_t)
        Temp[i] = (aW * Temp_old[i - 1] + aE * Temp_old[i + 1] + (aP - (aW + aE)) * Temp_old[i] + Su) / float(aP)

    # Boundary node 0
    aW = 0
    aE = k / float(del_x)
    Su = 0
    aP = rho_c * del_x / float(del_t)
    Temp[0] = (aE * Temp_old[1] + (aP - (aW + aE)) * Temp_old[0] + Su) / float(aP)

    # Boundary node L
    aE = 0
    aW = k / float(del_x)
    Su = (2.0 * k / float(del_x)) * (T_L - Temp_old[-1])
    aP = rho_c * del_x / float(del_t)
    Temp[-1] = (aW * Temp_old[-2] + (aP - (aW + aE)) * Temp_old[-1] + Su) / float(aP)

    for i in range(len(Temp)):
        Temp_old[i] = Temp[i]

print ("\n The temperature distribution is: \n" + str(Temp))

plt.plot(x_gr, Temp)
plt.xlabel("Length")
plt.ylabel("Temperature")
plt.title("Explicit scheme")
plt.show()
