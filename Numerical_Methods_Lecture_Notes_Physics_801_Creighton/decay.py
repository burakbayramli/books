import pylab

nuclei0 = input('initial number of nuclei -> ')
tau = input('decay time constant -> ')
dt = input('time step -> ')
tmax = input('time to end of simulation -> ')
nsteps = int(tmax/dt)
nuclei = [0.0]*nsteps
t = [0.0]*nsteps

# use Euler's method to integrate equation for radioactive decay
t[0] = 0.0
nuclei[0] = nuclei0
for i in range(nsteps-1):
    t[i+1] = t[i]+dt
    nuclei[i+1] = nuclei[i]-nuclei[i]/tau*dt
pylab.plot(t, nuclei, 'o-b')
pylab.xlabel('time')
pylab.ylabel('nuclei')
pylab.title('radioactive decay')
pylab.grid()
pylab.show()
