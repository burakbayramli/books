import math, pylab, random

J = 1.0  # exchange energy
L = input('number of atoms per side of lattice -> ')
nsweep = input('number sweeps to average -> ')
seed = input('random number seed -> ')
random.seed(seed)
N = L**2
kT = pylab.arange(0.1, 5.0, 0.1)
e = pylab.zeros(len(kT))
m = pylab.zeros(len(kT))
c = pylab.zeros(len(kT))

# initial data
s = pylab.ones((L, L))
E = 0.0
M = 0.0
for i in range(L):
    for j in range(L):
        E -= J*s[i,j]*(s[(i+1)%L,j]+s[i,(j+1)%L])
        M += s[i,j]

# prepare animated plot
pylab.ion()
image = pylab.imshow(s, vmax=1, vmin=-1)

# slowly warm up
for t in range(len(kT)):
    # average nsweep sweeps
    for sweep in range(nsweep):

        # update animated plot
        image.set_data(s)
        pylab.title('kT = %g'%kT[t])
        pylab.draw()

        # sweep over all particles in lattice
        for i in range(L):
            for j in range(L):

                # compute energy required to flip spin
		dE = s[(i+1)%L,j]+s[(i-1)%L,j]+s[i,(j+1)%L]+s[i,(j-1)%L]
                dE *= 2.0*J*s[i,j]

                # Metropolis algorithm to see if we should accept trial
                if dE <= 0.0 or random.random() <= math.exp(-dE/kT[t]):
                    # accept trial: reverse spin; return dE and dM
                    s[i,j] *= -1
                    M += 2.0*s[i,j]
                    E += dE

        # update running means and variances
        deltae = E-e[t]
        deltam = M-m[t]
        e[t] += deltae/(sweep+1)
        m[t] += deltam/(sweep+1)
        c[t] += deltae*(E-e[t])
    e[t] /= N
    m[t] /= N
    c[t] /= nsweep*N*kT[t]**2

# produce plots
pylab.ioff()
pylab.figure()
pylab.plot(kT, e, 'o')
pylab.xlabel('temperature')
pylab.ylabel('energy per atom')
pylab.grid()
pylab.figure()
pylab.plot(kT, m, 'o')
pylab.xlabel('temperature')
pylab.ylabel('magnetization per atom')
pylab.grid()
pylab.figure()
pylab.plot(kT, c, 'o')
pylab.xlabel('temperature')
pylab.ylabel('heat capacity per atom')
pylab.grid()
pylab.show()
