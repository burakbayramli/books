from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt
from orbit import *

# study convergence by looking at the displacement after 1 period

# circular orbit
o = Orbit(1.0, 0.0)   # eccentricity = 0

# orbital period
P = o.kepler_period()

# to properly measure convergence, with equal timestep, we want to
# make sure that dt * integer = P exactly, so there is no overshooting
N = 20

tstep = []
err_euler = []
err_ec = []
err_rk2 = []
err_rk4 = []

dt = P/N

for n in range(6):

    hist_Euler = o.int_Euler(dt, P)
    hist_Euler_Cromer = o.int_Euler_Cromer(dt, P)
    hist_RK2 = o.int_RK2(dt, P)
    hist_RK4 = o.int_RK4(dt, P)

    # error is final radius - initial radius.  Since we are circular, the
    # initial radius is o.a, the semimajor axis
    print(dt,
          abs(hist_Euler.displacement()),
          abs(hist_Euler_Cromer.displacement()),
          abs(hist_RK2.displacement()),
          abs(hist_RK4.displacement()))

    tstep.append(dt)
    err_euler.append(abs(hist_Euler.displacement()))
    err_ec.append(abs(hist_Euler_Cromer.displacement()))
    err_rk2.append(abs(hist_RK2.displacement()))
    err_rk4.append(abs(hist_RK4.displacement()))

    dt /= 2.0


plt.scatter(np.array(tstep), np.array(err_euler), label="Euler")
plt.plot(np.array(tstep), err_euler[0]*(tstep[0]/np.array(tstep))**-1)

plt.scatter(np.array(tstep), np.array(err_ec), label="Euler-Cromer")
plt.plot(np.array(tstep), err_ec[0]*(tstep[0]/np.array(tstep))**-1)

plt.scatter(np.array(tstep), np.array(err_rk2), label="R-K 2")
plt.plot(np.array(tstep), err_rk2[0]*(tstep[0]/np.array(tstep))**-2)

plt.scatter(np.array(tstep), np.array(err_rk4), label="R-K 4")
plt.plot(np.array(tstep), err_rk4[0]*(tstep[0]/np.array(tstep))**-4)

leg = plt.legend(frameon=False, fontsize="small")


ax = plt.gca()
ax.set_xscale('log')
ax.set_yscale('log')

plt.xlabel(r"$\tau$")
plt.ylabel("absolute error in displacement after one period")

plt.ylim(1.e-10, 10)

plt.tight_layout()
plt.savefig("orbit-converge-d.png", dpi=150)
