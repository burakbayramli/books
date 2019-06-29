import math, pylab

# fixed parameters
T0 = 25.0  # temperature gradient, C
D = 1e-4  # thermal diffusivity, m^2/s
L = 1.0  # length of rod, m

# input parameters
dx = L*input('grid spacing in units of rod length (L) -> ')
dt = (L**2/D)*input('time step in units of (L^2/D) -> ')
tmax = (L**2/D)*input('evolution time in units of (L^2/D) -> ')

# coefficients of the tridiagonal matrix
alpha = gamma = -D*dt/dx**2
beta = 1.0+2.0*D*dt/dx**2

# construct initial data
N = int(L/dx)
x = [0.0]*(N+1)
v = [0.0]*(N+1)
u = [0.0]*(N+1)
for j in range(N+1):
    x[j] = j*dx
u[0] = T0

# prepare animated plot
pylab.ion()
(line, ) = pylab.plot(x, u, '-k')
pylab.ylim(0, T0)
pylab.xlabel('x (m)')
pylab.ylabel('Temperature (Celcius)')

# preform the evolution
t = 0.0
while t < tmax:
    # update plot
    line.set_ydata(u)
    pylab.title('t = %5f'%t)
    pylab.draw()
    pylab.pause(0.1)

    # swap u and v
    (u, v) = (v, u)

    # boundary conditions
    u[0] = T0
    u[N] = 0.0

    # set the j=1 and j=N-1 points of v to the correct values
    v[1] -= alpha*u[0]
    v[N-1] -= gamma*u[N]

    # forward sweep
    u[1] = v[1]/beta
    v[1] = gamma/beta
    for j in range(2, N):
        den = beta-alpha*v[j-1]
        u[j] = (v[j]-alpha*u[j-1])/den
        v[j] = gamma/den
    # backward sweep
    for j in reversed(range(1, N-1)):
        u[j] -= u[j+1]*v[j]
    t += dt

pylab.ioff()
pylab.show()
