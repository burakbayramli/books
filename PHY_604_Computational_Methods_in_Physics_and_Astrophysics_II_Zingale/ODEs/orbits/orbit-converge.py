from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt
from orbit import *

# circular orbit
o = Orbit(1.0, 0.0)   # eccentricity = 0

# orbital period
P = o.kepler_period()

tstep = []
err_euler = []
err_rc = []
err_rk2 = []
err_rk4 = []

dt = 0.05

for n in range(6):
    hist_Euler = o.int_Euler(dt, P)
    hist_Euler_Cromer = o.int_Euler_Cromer(dt, P)
    hist_RK2 = o.int_RK2(dt, P)
    hist_RK4 = o.int_RK4(dt, P)

    # error is final radius - initial radius.  Since we are circular, the
    # initial radius is o.a, the semimajor axis
    print(dt,
          abs(hist_Euler.finalR()-o.a),
          abs(hist_Euler_Cromer.finalR()-o.a),
          abs(hist_RK2.finalR()-o.a),
          abs(hist_RK4.finalR()-o.a))

    tstep.append(dt)
    err_euler.append(abs(hist_Euler.finalR()-o.a))
    err_rc.append(abs(hist_Euler_Cromer.finalR()-o.a))
    err_rk2.append(abs(hist_RK2.finalR()-o.a))
    err_rk4.append(abs(hist_RK4.finalR()-o.a))

    dt /= 2


plt.scatter(np.array(tstep), np.array(err_euler), label="Euler")
plt.plot(np.array(tstep), err_euler[0]*(tstep[0]/np.array(tstep))**-1)

plt.scatter(np.array(tstep), np.array(err_rc), label="Euler-Cromer")
plt.plot(np.array(tstep), err_rc[0]*(tstep[0]/np.array(tstep))**-1)

plt.scatter(np.array(tstep), np.array(err_rk2), label="R-K 2")
plt.plot(np.array(tstep), err_rk2[0]*(tstep[0]/np.array(tstep))**-2)

plt.scatter(np.array(tstep), np.array(err_rk4), label="R-K 4")
plt.plot(np.array(tstep), err_rk4[0]*(tstep[0]/np.array(tstep))**-4)

leg = plt.legend(frameon=False, fontsize="small")

ax = plt.gca()
ax.set_xscale('log')
ax.set_yscale('log')

plt.xlabel(r"$\tau$")
plt.ylabel("absolute error in radius after one period")

plt.ylim(1.e-10, 10)

plt.tight_layout()
plt.savefig("orbit-converge.png", dpi=150)
