import pylab

g = 9.8  # standard freefall (m/s^2)
l = input('pendulum length (meters) -> ')
theta0 = input('initial angle (radians) -> ')
dt = input('time step (seconds) -> ')
tmax = input('time to end of simulation (seconds) -> ')
nsteps = int(tmax/dt)
omega = [0.0]*nsteps
theta = [0.0]*nsteps
t = [0.0]*nsteps

# use Cromer's method to integrate pendulum equations of motion
theta[0] = theta0
for i in range(nsteps-1):
    omega[i+1] = omega[i]-g/l*theta[i]*dt
    theta[i+1] = theta[i]+omega[i+1]*dt
    t[i+1] = t[i]+dt
pylab.plot(t, theta)
pylab.xlabel('time (s)')
pylab.ylabel('theta (rad)')
pylab.title('simple pendulum (Cromer method)')
pylab.show()
