import math, cmath, pylab

hbar = 1.0  # reduced Planck's constant
m = 1.0  # mass
k = 1.0  # spring constant

# grid and time intervals
dx = 0.01
dt = 0.05
tmax = 10.0
xmin = -5.0
xmax = 5.0
N = int((xmax-xmin)/dx)

# initial data
x = [0.0]*(N+1)
u = [0.0]*(N+1)
v = [0.0]*(N+1)
p = [0.0]*(N+1)
omega0 = (k/m)**0.5
sigma = (hbar/(2.0*m*omega0))**0.5
for j in range(N+1):
    x[j] = xmin+j*dx
    u[j] = math.exp(-x[j]**2/(4.0*sigma**2))
    u[j] = u[j]/(2.0*math.pi*sigma**2)**0.25
    p[j] = u[j].real**2+u[j].imag**2

# potential
E0 = 0.5*hbar*omega0
V = [0.0]*(N+1)
for j in range(N+1):
    V[j] = 0.5*k*x[j]**2


# setup coefficients of the tridiagonal matrix
alpha = gamma = -1j*hbar*dt/(4.0*m*dx**2)
beta = [0.0]*(N+1)
for j in range(N):
    beta[j] = 1.0-2.0*alpha+1j*(V[j]/(2.0*hbar))*dt

# prepare animated plot
pylab.ion()
fig = pylab.figure()
ax1 = fig.add_subplot(111)
ax1.set_xlim(xmin, xmax)
ax1.set_ylim(0.0, 1.1*max(p))
ax1.set_xlabel('x')
ax1.set_ylabel('probability density')
ax2 = ax1.twinx()
ax2.set_xlim(xmin, xmax)
ax2.set_ylim(0.0, 1.1*max(V)/E0)
ax2.set_ylabel('V / E0')

# plot potential function and wave function
ax2.plot(x, [Vj/E0 for Vj in V], 'b')
(line, ) = ax1.plot(x, p, 'k-')

# preform the evolution
t = 0.0
while t-tmax < 0.5*dt:
    # update plot
    for j in range(N+1):
        p[j] = u[j].real**2+u[j].imag**2
    line.set_ydata(p)
    pylab.title('t = %5f'%t)
    pylab.draw()

    # set the values of the rhs
    for j in range(1, N):
        v[j] = -alpha*u[j-1]+(2.0-beta[j])*u[j]-gamma*u[j+1]
    v[1] -= alpha*u[0]
    v[N-1] -= gamma*u[N]

    # forward sweep
    u[1] = v[1]/beta[1]
    v[1] = gamma/beta[1]
    for j in range(2, N):
        den = beta[j]-alpha*v[j-1]
        u[j] = (v[j]-alpha*u[j-1])/den
        v[j] = gamma/den
    # backward sweep
    for j in reversed(range(1, N)):
        u[j] -= u[j+1]*v[j]
    t += dt

# freeze final plot
pylab.ioff()
pylab.draw()
pylab.show()
