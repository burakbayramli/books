import math, pylab

GM = (2.0*math.pi)**2  # heliocentric gravitational constant


# function that implements rk4 integration
def rk4(t, x, f, dt):
    dx1 = f(t, x)*dt
    dx2 = f(t+0.5*dt, x+0.5*dx1)*dt
    dx3 = f(t+0.5*dt, x+0.5*dx2)*dt
    dx4 = f(t+dt, x+dx3)*dt
    return x+dx1/6.0+dx2/3.0+dx3/3.0+dx4/6.0


# function that returns dX/dt for the orbital equations of motion
def dXdt(t, X):
    x = X[0]
    vx = X[1]
    y = X[2]
    vy = X[3]
    r = math.sqrt(x**2+y**2)
    ax = -GM*x/r**3
    ay = -GM*y/r**3
    return pylab.array([vx, ax, vy, ay])


x0 = input('initial x position (au) -> ')
y0 = input('initial y position (au) -> ')
vx0 = input('initial x velocity (au/yr) -> ')
vy0 = input('initial y velocity (au/yr) -> ')
dt = input('time step (yr) -> ')
tmax = input('time to end of simulation (yr) -> ')
nsteps = int(tmax/dt)
x = [0.0]*nsteps
y = [0.0]*nsteps

# integrate Newton's equations of motion using rk4;
# X is a vector that contains the positions and velocities being integrated
X = pylab.array([x0, vx0, y0, vy0])
for i in range(nsteps):
    x[i] = X[0]
    y[i] = X[2]
    # update the vector X to the next time step
    X = rk4(i*dt, X, dXdt, dt)
pylab.figure(figsize=(6, 6))
pylab.plot(x, y, 'o-')
pylab.xlabel('x (au)')
pylab.ylabel('y (au)')
minmax = 1.1*max(abs(min(x+y)), abs(max(x+y)))
pylab.axis([-minmax, minmax, -minmax, minmax], aspect='equal')
pylab.grid()
pylab.show()
