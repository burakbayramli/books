import numpy as np
from matplotlib import pyplot as plt


# Peclet function scheme
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
x_points = 11
y_points = 11
del_x = x_len/float(x_points-1)
del_y = y_len/float(y_points-1)

x = np.arange(x_points+1)
y = np.arange(y_points+1)
f = 0.5
x_w = np.arange(x[1] - f, x[-2], 1)
x_e = np.arange(x[1] + f, x[-1], 1)
y_s = np.arange(y[1] - f, y[-2], 1)
y_n = np.arange(y[1] + f, y[-1], 1)

u = np.zeros((x_points-1, y_points-1))
v = np.zeros((x_points-1, y_points-1))
u_star = np.zeros((x_points-1, y_points-1))
v_star = np.zeros((x_points-1, y_points-1))

P = np.zeros((x_points, y_points))
P_star = np.zeros((x_points, y_points))
P_corr = np.zeros((x_points, y_points))

# Boundary conditions
u[0,:] = 10
v[:,0] = 11
P[0,:] = 20
P[-1,:] = 10

rho = 1
Sc = 50      # Linearization of source term
Sp = 0
Gamma = 1    # Assuming equal Gamma (diffusive coefficient) throughout the domain
n = 1        # Power scheme
alpha = 1  # Relaxation factor
n_itrs = 100

for itrs in range(n_itrs):
    for i in range(1, x_points-2):
        for j in range(1, y_points-2):
            del_x_e = x[i + 1] - x[i]
            del_x_w = x[i] - x[i - 1]
            del_y_s = y[j] - y[j - 1]
            del_y_n = y[j + 1] - y[j]

            De, Dw = Gamma * del_y / float(del_x_e), Gamma * del_y / float(del_x_w)
            Dn, Ds = Gamma * del_x / float(del_y_n), Gamma * del_x / float(del_y_s)
            Dpe, Dpn = Gamma * del_y / float(del_x), Gamma * del_x / float(del_y)
            Fe, Fw = rho * u[i+1,j] * del_y, rho * u[i-1,j] * del_y
            Fn, Fs = rho * v[i,j+1] * del_x, rho * v[i,j-1] * del_x
            Fpe, Fpn =  rho * u[i,j] * del_y, rho * v[i,j] * del_x
            Pe, Pw = Fe / float(De), Fw / float(Dw)
            Pn, Ps = Fn / float(Dn), Fs / float(Ds)
            Ppe, Ppn = Fpe / float(Dpe), Fpn / float(Dpn)

            aE = De * funcPeclet(Pe, n) + max(-1 * Fe, 0)
            aW = Dw * funcPeclet(Pw, n) + max(-1 * Fw, 0)
            aN = Dn * funcPeclet(Pn, n) + max(-1 * Fn, 0)
            aS = Ds * funcPeclet(Ps, n) + max(-1 * Fs, 0)
            aP_e, aP_n = Dpe * funcPeclet(Ppe, n) + max(-1 * Fpe, 0), Dpn * funcPeclet(Ppn, n) + max(-1 * Fpn, 0)
            b = Sc * del_x * del_y

            u_star[i,j] = ((aE * u[i + 1, j] + aW * u[i - 1, j] + aN * v[i, j + 1] + aS * v[i, j - 1]) + b + (
                        P[i, j] - P[i + 1, j]) * del_y) / float(aP_e)
            v_star[i,j] = ((aE * u[i + 1, j] + aW * u[i - 1, j] + aN * v[i, j + 1] + aS * v[i, j - 1]) + b + (
                    P[i, j] - P[i, j+1]) * del_x) / float(aP_n)

            d_e = del_y/float(aP_e)
            d_w = d_e
            d_n = del_x/float(aP_n)
            d_s = d_n

            aE = rho * d_e * del_y
            aW = rho * d_w * del_y
            aN = rho * d_n * del_x
            aS = rho * d_s * del_x
            aP = aE + aW + aN + aS
            b1 = rho * (u_star[i, j] - u_star[i + 1, j]) * del_y + rho * (v_star[i, j] - v_star[i, j + 1]) * del_x

            P_corr[i,j] = (aE*P_corr[i+1, j] + aW*P_corr[i-1,j] + aN*P[i,j+1] + aS*P[i,j-1] + b1)/float(aP)
            P[i,j] = P_star[i,j] + alpha*P_corr[i,j]

            u[i, j] = u_star[i, j] + d_e * (P_corr[i, j] - P_corr[i + 1, j])
            v[i, j] = v_star[i, j] + d_n * (P_corr[i, j] - P_corr[i, j + 1])

    for i in range(0, x_points):
        for j in range(0, y_points):
            P_star[i,j] = P_corr[i,j]


print ("\n Pressure distribution is: \n" + str(P))
print ("\n The max pressure is: \t" + str(P.max()))

xx = np.linspace(0, x_len, x_points+1)
yy = np.linspace(0, y_len, y_points+1)
cmap = plt.pcolormesh(xx, yy, P)     # https://scientific-python-101.readthedocs.io/matplotlib/pcolormesh_plots.html
plt.colorbar(cmap)
plt.show()

