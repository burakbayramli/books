# The test potential here comes from Pang.
#
# This version seems to work for both odd and even parity solutions
# and looks like it reproduces the sample in Pang (note, we differ
# by a factor of 1/2 in our constants).

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt
import schrodinger

def V(x):
    """ the potential well """
    # this is Pang Eq. 4.97
    alpha = 1.0
    lambdac = 4.0

    return alpha**2*lambdac*(lambdac - 1.0)*(0.5 - 1.0/np.cosh(alpha*x)**2)


def Ev(n):
    """ the analytic form of the eigenvalues for our potential """
    # this is Pang Eq. 4.98
    alpha = 1.0
    lambdac = 4.0

    return alpha**2*(0.5*lambdac*(lambdac - 1.0) - (lambdac - 1.0 - n)**2)


# pick a starting point far from the action
x0 = 10.0
x_match = 1.0

nsteps = 500

s = schrodinger.Schrodinger(V, x_match=x_match, x_far=x0, nsteps=nsteps)


print("expected eigenvalues: ")
for n in range(10):
    print(n, Ev(n))


# gives E = -3 -- ground state
E_old = -2.9
E_new = -2.95

# gives E = 2 -- first excited state
#E_old = -2.3
#E_new = -2.5

# gives E = 5 -- second excited state
#E_old = 4.9
#E_new = 4.95

# the next eigenvalue by the analytic expression is E = 6, but this
# is the top of the potential well, so this is not a bound state


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
plt.savefig("complex-well-psi-E{}.png".format(ep.strip()))


