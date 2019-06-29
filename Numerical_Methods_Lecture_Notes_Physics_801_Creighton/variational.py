import math, random, pylab

# parameters for harmonic oscillator
hbar = 1.0
m = 1.0
k = 1.0
omega0 = (k/m)**0.5

# input number of grid points, steps, random seed
N = input('number of grid points -> ')
nstep = input('number of steps -> ')
seed = input('random number seed -> ')
random.seed(seed)

# setup grid and initial guess
xmin = -5.0
xmax = 5.0
dx = (xmax-xmin)/(N-1)
x = pylab.arange(xmin, xmax+0.1*dx, dx)
psi = pylab.ones(N)  # initial guess
psi[0] = psi[N-1] = 0.0  # endpoints fixed at zero

# compute energy, potential, normalization
V = pylab.zeros(N)
E = pylab.zeros(nstep+1)
ssq = 0
for i in range(1, N-1):
    V[i] = k*x[i]**2/2.0
    H = -hbar**2*(psi[i-1]-2.0*psi[i]+psi[i+1])/(2*m*dx**2)
    H += V[i]*psi[i]
    E[0] += psi[i]*H*dx
    ssq += psi[i]**2*dx
E[0] /= ssq
psi /= ssq**0.5

# prepare animated plot
pylab.ion()
xfine = pylab.arange(xmin, xmax, 0.01)
psi0 = [(m*omega0/(math.pi*hbar))**0.25*math.exp(-0.5*m*omega0*xx**2/hbar)
        for xx in xfine]
pylab.plot(xfine, psi0)
(line, ) = pylab.plot(x, psi, 'o-')
pylab.ylabel('$\psi$')
pylab.xlabel('x')

# perform the evolution
n = 1
while n <= nstep:
    # choose a random point and a random amount to change psi
    tmp = pylab.copy(psi)  # temporary wavefunction trial
    j = random.choice(range(1, N-1))
    tmp[j] *= random.uniform(0.8, 1.2)

    # normalize and compute energy
    E[n] = 0.0
    ssq = 0.0
    for i in range(1, N-1):
        H = -hbar**2*(tmp[i-1]-2.0*tmp[i]+tmp[i+1])/(2*m*dx**2)
        H += V[i]*tmp[i]
        E[n] += tmp[i]*H*dx
        ssq += tmp[i]**2*dx
    E[n] /= ssq

    # test if the trial wavefunction reduces energy
    if E[n] < E[n-1]:
        # update current wavefunction
        psi = tmp/ssq**0.5

        # update plot
        line.set_ydata(psi)
        pylab.title('%d moves'%n)
        pylab.draw()

        # increment step count
        n += 1

# freeze animation and plot energy as a function of time
pylab.ioff()
pylab.figure()
pylab.plot(range(nstep+1), E)
pylab.ylabel('$E / \hbar\omega_0$')
pylab.xlabel('step number')
pylab.grid()
pylab.show()
