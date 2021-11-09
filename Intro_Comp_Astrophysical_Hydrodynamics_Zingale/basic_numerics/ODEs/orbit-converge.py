import numpy
import pylab
import math
from orbit import *

# circular orbit
o = Orbit(1.0, 0.0)   # eccentricity = 0

# orbital period
P = o.kepler_period()

tstep = []
err_Euler = []
err_EC = []
err_RK2 = []
err_RK4 = []

dt = 0.05

for i in range(5):
    hist_Euler = o.int_Euler(dt, P)
    hist_Euler_Cromer = o.int_Euler_Cromer(dt, P)
    hist_RK2 = o.int_RK2(dt, P)
    hist_RK4 = o.int_RK4(dt, P)

    # error is final radius - initial radius.  Since we are circular, the
    # initial radius is o.a, the semimajor axis
    print dt, \
        abs(hist_Euler.final_R()-o.a), \
        abs(hist_Euler_Cromer.final_R()-o.a), \
        abs(hist_RK2.final_R()-o.a), \
        abs(hist_RK4.final_R()-o.a)

    tstep.append(dt)
    err_Euler.append(abs(hist_Euler.final_R()-o.a))
    err_EC.append(abs(hist_Euler_Cromer.final_R()-o.a))
    err_RK2.append(abs(hist_RK2.final_R()-o.a))
    err_RK4.append(abs(hist_RK4.final_R()-o.a))

    dt /= 2

pylab.scatter(numpy.array(tstep), numpy.array(err_Euler), label="Euler", color="k")
pylab.plot(numpy.array(tstep), err_Euler[0]*(tstep[0]/numpy.array(tstep))**-1, color="k")

pylab.scatter(numpy.array(tstep), numpy.array(err_EC), label="Euler-Cromer", color="r")
pylab.plot(numpy.array(tstep), err_EC[0]*(tstep[0]/numpy.array(tstep))**-1, color="r")

pylab.scatter(numpy.array(tstep), numpy.array(err_RK2), label="R-K 2", color="b")
pylab.plot(numpy.array(tstep), err_RK2[0]*(tstep[0]/numpy.array(tstep))**-2, color="b")

pylab.scatter(numpy.array(tstep), numpy.array(err_RK4), label="R-K 4", color="g")
pylab.plot(numpy.array(tstep), err_RK4[0]*(tstep[0]/numpy.array(tstep))**-4, color="g")

leg = pylab.legend(loc=2)
ltext = leg.get_texts()
pylab.setp(ltext, fontsize='small')
leg.draw_frame(0)

ax = pylab.gca()
ax.set_xscale('log')
ax.set_yscale('log')

pylab.xlabel(r"$\tau$")
pylab.ylabel("absolute error in radius after one period")

pylab.ylim(1.e-10, 10)

pylab.savefig("orbit-converge.png")

