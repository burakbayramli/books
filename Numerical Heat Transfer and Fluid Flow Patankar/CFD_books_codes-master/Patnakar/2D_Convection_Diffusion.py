import numpy as np
from matplotlib import pyplot as plt


# Define scheme formulation for Peclet number
def funcPeclet(P, n):
    if n == 1:
        # Central Difference
        return 1 - 0.5*np.mod(P, 1)
    if n == 2:
        # Upwind
        return 1
    if n == 3:
        # Hybrid
        return max(0, 1 - (0.1 * pow(np.mod(P, 1), 1)))
    if n == 4:
        # Power law
        return max(0, 1 - (0.1 * pow(np.mod(P, 1), 5)))
    else:
        # Return power law by default
        return max(0, 1 - (0.1 * pow(np.mod(P, 1), 5)))


# Define the domain
x_len = 8
y_len = 8
x_points = 101
y_points = 101

del_x = x_len/float(x_points - 1)
del_y = x_len/float(y_points - 1)
Sc = 50      # Linearization of source term
Sp = 0
Gamma = 1        # Assuming equal Gamma (diffusive coefficient) throughout the domain
rho = 1          # Assuming equal density throughout the domain
vel_u = 1        # Velocity of flow along X direction
vel_v = 4        # Velocity of flow along Y direction

x = np.arange(x_points+1)
y = np.arange(y_points+1)
f = 0.5     # Assuming equal fraction in the control volume shared between two adjacent grids
x_w = np.arange(x[1] - f, x[-2], 1)
x_e = np.arange(x[1] + f, x[-1], 1)
y_s = np.arange(y[1] - f, y[-2], 1)
y_n = np.arange(y[1] + f, y[-1], 1)

flux = np.zeros((x_points, y_points))

# Discretization equation
num_itrs = 1000     # Number of iterations for Gauss-Siedel method
n = 4               # Choose scheme formulation from table above

# Applying boundary conditions
flux[:,0] = 100
flux[0,:] = 100
flux[:,-1] = 0
flux[-1,:] = 0

for nitr in range(num_itrs):
    for i in range(1, x_points-1):
        for j in range(1, y_points-1):
            del_x_e = x[i + 1] - x[i]
            del_x_w = x[i] - x[i - 1]
            del_y_s = y[j] - y[j - 1]
            del_y_n = y[j + 1] - y[j]

            De, Dw = Gamma*del_y/float(del_x_e), Gamma*del_y/float(del_x_w)
            Dn, Ds = Gamma*del_x/float(del_y_n), Gamma*del_x/float(del_y_s)
            Fe, Fw = rho*vel_u*del_y, rho*vel_u*del_y
            Fn, Fs = rho*vel_v*del_x, rho*vel_v*del_x
            Pe, Pw = Fe/float(De), Fw/float(Dw)
            Pn, Ps = Fn/float(Dn), Fs/float(Ds)

            aE = De * funcPeclet(Pe, n) + max(-1 * Fe, 0)
            aW = Dw * funcPeclet(Pw, n) + max(-1 * Fw, 0)
            aN = Dn * funcPeclet(Pn, n) + max(-1 * Fn, 0)
            aS = Ds * funcPeclet(Ps, n) + max(-1 * Fs, 0)
            aP = aE + aW + aN + aS - (Sp * del_x * del_y)
            b = Sc * del_x * del_y

            flux[i,j] = (aW * flux[i - 1,j] + aE * flux[i + 1,j] + aN * flux[i, j + 1] + aS * flux[i, j - 1] + b) / float(aP)

print ("\n Flux distribution is: \n" + str(flux))
print ("\n The max flux is: \t" + str(flux.max()))

xx = np.linspace(0, x_len, x_points+1)
yy = np.linspace(0, y_len, y_points+1)
cmap = plt.pcolormesh(xx, yy, flux)     # https://scientific-python-101.readthedocs.io/matplotlib/pcolormesh_plots.html
plt.colorbar(cmap)
plt.show()
