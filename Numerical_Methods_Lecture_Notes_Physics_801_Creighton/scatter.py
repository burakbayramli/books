import math, cmath, pylab

# solves the tridiagonal system of equations A.x = b
def tridiag(alp, bet, gam, b):
    n = len(bet)
    x = pylab.zeros(b.shape, dtype=complex)
    y = pylab.zeros(b.shape, dtype=complex)
    y[0] = gam[0]/bet[0]
    x[0] = b[0]/bet[0]
    for i in range(1, n):
        den = bet[i]-alp[i]*y[i-1]
        y[i] = gam[i]/den
        x[i] = (b[i]-alp[i]*x[i-1])/den
    for i in reversed(range(n-1)):
        x[i] -= x[i+1]*y[i]
    return x


hbar = 1.0  # reduced Planck's constant
m = 1.0  # mass
k0 = 1.0  # initial wavenumber

# grid and time intervals
dy = dx = 0.5/k0
dt = 0.5*m/(hbar*k0**2)
tmax = 30.0*m/(hbar*k0**2)
ymin = xmin = -30.0/k0
ymax = xmax = 30.0/k0

# initial data
x = pylab.arange(xmin, xmax, dx)
y = pylab.arange(ymin, ymax, dy)
N = len(x)
u = pylab.zeros((N, N), dtype=complex)
v = pylab.zeros((N, N), dtype=complex)
p = pylab.zeros((N, N))
x0 = -15.0/k0
y0 = 0.0/k0
sigma = 5.0/k0
for j in range(N):
    for k in range(N):
        rr = (x[j]-x0)**2+(y[k]-y0)**2
        u[j,k] = cmath.exp(-rr/(4.0*sigma**2)+1j*k0*x[j])
        p[j,k] = u[j,k].real**2+u[j,k].imag**2

# potential
a = 2.0/k0
E0 = (hbar*k0)**2/(2.0*m)
V = pylab.zeros((N, N))
for j in range(N):
    for k in range(N):
        rr = x[j]**2+y[k]**2
        if rr < a**2:
            V[j,k] = E0

# prepare animated plot
pylab.ion()
pylab.xlabel('x')
pylab.ylabel('y')
image = pylab.imshow(p.T, origin='upper', extent=(xmin, xmax, ymin, ymax),
                     cmap=pylab.cm.hot)
pylab.contour(V.T, origin='upper', extent=(xmin, xmax, ymin, ymax), colors='c')

# setup coefficients of the tridiagonal matrix
alpha = gamma = -1j*hbar*dt/(4.0*m*dx**2)
alp = alpha*pylab.ones(N, dtype=complex)
gam = gamma*pylab.ones(N, dtype=complex)
bet = pylab.zeros((N, N), dtype=complex)
for j in range(N):
    for k in range(N):
        bet[j,k] = 1.0-2.0*alpha+1j*(V[j,k]/(2.0*hbar))*dt

# preform the evolution; blithely ignore boundary conditions
t = 0.0
while t-tmax < 0.5*dt:

    # update plot
    for j in range(N):
        for k in range(N):
            p[j,k] = u[j,k].real**2+u[j,k].imag**2
    image.set_data(p.T)
    pylab.title('t = %5f'%t)
    pylab.draw()

    # first step of operator splitting
    for j in range(1, N-1):
        for k in range(1, N-1):
            v[j,k] = -alpha*u[j,k-1]+(2.0-bet[j,k])*u[j,k]-gamma*u[j,k+1]
    for k in range(1, N-1):
        u[:,k] = tridiag(alp, bet[:,k], gam, v[:,k])

    # second step of operator splitting
    for k in range(1, N-1):
        for j in range(1, N-1):
            v[j,k] = -alp[j]*u[j-1,k]+(2.0-bet[j,k])*u[j,k]-gam[j]*u[j+1,k]
    for j in range(1, N-1):
        u[j,:] = tridiag(alp, bet[j,:], gam, v[j,:])
    t += dt

# freeze final plot
pylab.ioff()
pylab.draw()
pylab.show()
