import math, pylab

# fixed parameters
T0 = 25.0  # temperature gradient, C
D = 1e-4  # thermal diffusivity, m^2/s
L = 1.0  # length of rod, m

# input parameters
dx = L*input('grid spacing in units of rod length (L) -> ')
dt = dx**2/D*input('time step in units of (dx^2/D) -> ')
tmax = L**2/D*input('evolution time in units of (L^2/D) -> ')

# construct initial data
N = int(L/dx)
x = [0.0]*(N+1)
u0 = [0.0]*(N+1)
u1 = [0.0]*(N+1)
for j in range(N+1):
    x[j] = j*dx

# prepare animated plot
pylab.ion()
(line, ) = pylab.plot(x, u0, '-k')
pylab.ylim(0, T0)
pylab.xlabel('x (m)')
pylab.ylabel('Temperature (Celcius)')

# preform the evolution
t = 0.0
while t < tmax:
    # update plot
    line.set_ydata(u0)
    pylab.title('t = %5f'%t)
    pylab.draw()
    pylab.pause(0.1)

    # derivatives at interior points
    for j in range(1, N):
        u1[j] = u0[j]+dt*D*(u0[j+1]-2.0*u0[j]+u0[j-1])/dx**2

    # boundary conditions
    u1[0] = T0
    u1[N] = 0.0

    # swap old and new lists
    (u0, u1) = (u1, u0)
    t += dt

# freeze final plot
pylab.ioff()
pylab.show()
