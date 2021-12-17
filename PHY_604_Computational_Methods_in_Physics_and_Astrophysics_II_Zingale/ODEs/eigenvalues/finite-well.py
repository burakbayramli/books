from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt
import schrodinger

def V(x):
    """ the potential well -- a finite square well """
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

# some examples
for n, (E_old, E_new) in enumerate([(-970, -960), (-980, -975), (-990, -980), (-999, -998), (-100, -150)]):
    if n == 4:
        E, xi_s, psi_s = s.solve(E_old, E_new, plot_intermediate=True)
    else:
        E, xi_s, psi_s = s.solve(E_old, E_new)

    print("eigenvalue is E = {}".format(E))

    Vs = V(xi_s)

    plt.clf()

    plt.plot(xi_s, Vs/max(np.abs(Vs)), lw=2, label=r"$V(x)/\max{V(x)}$")
    plt.plot(xi_s, psi_s/max(np.abs(psi_s)), label=r"$\psi(x)/\max{\psi(x)}$")

    plt.xlabel("x")

    plt.title(r"eigenvalue, $\mathcal{{E}}$ = {}".format(E))

    plt.xlim(-0.5*x0, 0.5*x0)
    plt.ylim(-1.2, 1.2)

    plt.legend(frameon=False, loc="best")

    ep = "{:6.2f}".format(E)
    plt.savefig("finite-well-psi-E{}.png".format(ep.strip()))

