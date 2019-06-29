import pylab, math, random

L = 100  # length of side of box
M = 20  # length of side of square of particles
B = 10  # number of coarse-grainig bins per side
nsteps = input('number of steps in walk -> ')
steps = range(nsteps)
seed = input('random number seed -> ')
random.seed(seed)

# initial positions of particles form a M*M block in the middle of the box
xside = range((L-M)//2, (L+M)//2)
yside = range((L-M)//2, (L+M)//2)
x = [i for i in xside for j in yside]  # x-locations of the particles
y = [j for i in xside for j in yside]  # y-locations of the particles
N = len(xside)*len(yside)  # number of particles
S = [0.0]*nsteps  # entropy
P = pylab.zeros((B, B))  # probability of particle being in each bin

# setup animated figure
pylab.figure(figsize=(6, 6))
(points, ) = pylab.plot(x, y, ',')
pylab.xlim(0, L)
pylab.ylim(0, L)
pylab.xticks(range(0, L+1, L//B))
pylab.yticks(range(0, L+1, L//B))
pylab.xlabel('x')
pylab.ylabel('y')
pylab.grid()

# simulate the random walks
for n in steps:
    # update plot
    points.set_data(x, y)
    pylab.title('step %d'%n)
    pylab.pause(1e-6)
    pylab.draw()

    # update positions of particles and update counts in bins
    P.fill(0)
    for i in range(N):
        (dx, dy) = random.choice([(-1, 0), (1, 0), (0, -1), (0, 1)])
        x[i] += dx
        y[i] += dy
        # make sure that the particles stay in the box
        if x[i] < 0 or x[i] >= 100:
            x[i] -= dx
        if y[i] < 0 or y[i] >= 100:
            y[i] -= dy
        # increment count in bin containing particle
        P[x[i]*B//L,y[i]*B//L] += 1.0

    # compute the entropy at this step
    for i in range(B):
        for j in range(B):
            P[i,j] /= N
            if P[i,j] > 0:
                S[n] -= P[i,j]*math.log(P[i,j])
pylab.figure()
pylab.plot(steps, S)
pylab.xlabel('step')
pylab.ylabel('entropy')
pylab.show()
