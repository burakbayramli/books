from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt
import schrodinger

def V(x):
    """ the potential well """
    idx = np.abs(x) < 1.0
    Vwell = np.zeros_like(x)
    Vwell[idx] = -1000.0
    return Vwell


# pick a starting point far from the action -- we need this pretty
# big
x0 = 5.0
x_match = 0.257

nsteps = 250

s = schrodinger.Schrodinger(V, x_match=x_match, x_far=x0, nsteps=nsteps)


# gives third excited state
E_old = -970
E_new = -960

# gives second excited state
#E_old = -980
#E_new = -975

# gives first excited state
#E_old = -990
#E_new = -980

# gives ground state
#E_old = -999
#E_new = -998


# high state
E_old = -100.
E_new = -150.

E, xi_s, psi_s = s.solve(E_old, E_new)

print("eigenvalue is E = {}".format(E))

Vs = V(xi_s)

plt.clf()

plt.plot(xi_s, Vs/max(np.abs(Vs)), color="b", lw=2, label=r"$V(x)/\max{V(x)}$")
plt.plot(xi_s, psi_s/max(np.abs(psi_s)), color="r", label=r"$\psi(x)/\max{\psi(x)}$")

plt.xlabel("x")

plt.title(r"eigenvalue, $\mathcal{{E}}$ = {}".format(E))

plt.xlim(-0.5*x0, 0.5*x0)

plt.ylim(-1.2, 1.2)

plt.legend(frameon=False, loc="best")

ep = "{:6.2f}".format(E)
plt.savefig("finite-well-psi-E{}.png".format(ep.strip()))

