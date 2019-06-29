import math, pylab

nuclei0 = input('initial number of nuclei -> ')
tau = input('decay time constant -> ')
dtlow = input('lowest resolution time step -> ')
nres = input('number of resolution refinements -> ')
tmax = input('time to end of simulation -> ')
for n in range(nres):
    refine = 10**n
    dt = dtlow/refine
    nsteps = int(tmax/dt)
    nuclei = nuclei0
    err = [0.0]*nsteps
    t = [0.0]*nsteps

    # use Euler's method to integrate equation for radioactive decay compute
    # error relative to exact solution
    for i in range(nsteps-1):
        t[i+1] = t[i]+dt
        nuclei = nuclei-nuclei/tau*dt
        exact = nuclei0*math.exp(-t[i+1]/tau)
        err[i+1] = abs((nuclei-exact)/exact)

    # plot the error at this resolution
    pylab.loglog(t[refine::refine], err[refine::refine], '.-', label='dt = '
                 +str(dt))
pylab.legend(loc=4)
pylab.xlabel('time')
pylab.ylabel('fractional error')
pylab.title('radioactive decay integration error')
pylab.grid(linestyle='-', which='major')
pylab.grid(which='minor')
pylab.show()
