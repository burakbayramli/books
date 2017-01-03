import numpy as np
import matplotlib.pyplot as plt

def remove_i(x, i):
    """Drops the ith element of an array."""
    shape = (x.shape[0]-1,) + x.shape[1:]
    y = np.empty(shape, dtype=float)
    y[:i] = x[:i]
    y[i:] = x[i+1:]
    return y

def a(i, x, G, m):
    """The acelleration of the ith mass."""
    x_i = x[i]
    x_j = remove_i(x, i)
    m_j = remove_i(m, i)
    diff = x_j - x_i
    mag3 = np.sum(diff**2, axis=1)**1.5
    result = G * np.sum(diff * (m_j / mag3)[:,np.newaxis], axis=0)
    return result

from multiprocessing import Pool

def timestep_i(args):
    """Computes the next position and velocity for the ith mass."""
    i, x0, v0, G, m, dt = args
    a_i0 = a(i, x0, G, m)
    v_i1 = a_i0 * dt + v0[i]
    x_i1 = a_i0 * dt**2 + v0[i] * dt + x0[i]
    return i, x_i1, v_i1

def timestep(x0, v0, G, m, dt, pool):
    """Computes the next position and velocity for all masses given 
    a initial conditions and a time step size.
    """
    N = len(x0)
    tasks = [(i, x0, v0, G, m, dt) for i in range(N)]
    results = pool.map(timestep_i, tasks)
    x1 = np.empty(x0.shape, dtype=float)
    v1 = np.empty(v0.shape, dtype=float)
    for i, x_i1, v_i1 in results:
        x1[i] = x_i1
        v1[i] = v_i1
    return x1, v1

def initial_cond(N, D):
    """Generates initial conditions for N unity masses at rest 
    starting at random positions in D-dimensional space.
    """
    x0 = np.random.rand(N, D)
    v0 = np.zeros((N, D), dtype=float)
    m = np.ones(N, dtype=float)
    return x0, v0, m    


def makefig(x, v, t):
    plt.clf()
    plt.plot(x[:,0], x[:,1], 'ro')
    if not np.all(v == 0.0):
        plt.quiver(x[:,0], x[:,1], v[:,0], v[:,1])
    plt.xlabel('x')
    plt.ylabel('y')
    plt.title('Time t = {0}'.format(t))
    plt.savefig('n-body-t{0}.svg'.format(t))

x0, v0, m = initial_cond(10, 2)
pool = Pool(4)
x1, v1 = timestep(x0, v0, 1.0, m, 1.0e-3, pool)
del pool

makefig(x0, v0, 0)
makefig(x1, v1, 1e-3)

def simulate(P, N, D, S, G, dt):
    x0, v0, m = initial_cond(N, D)
    pool = Pool(P)
    for s in range(S):
        x1, v1 = timestep(x0, v0, G, m, dt, pool)
        x0, v0 = x1, v1

import time
Ps = [1, 2, 4, 8]
runtimes = []
for P in Ps:
    print("running", P)
    start = time.time()
    simulate(P, 256, 3, 300, 1.0, 1e-3)
    stop = time.time()
    runtimes.append(stop - start)
print(runtimes)

rts = runtimes[0] / np.array(runtimes) 
plt.clf()
plt.plot(Ps, rts, 'ko-')
plt.axis([1, 8, 1, 2])
plt.xlabel('Number of Processes, $P$')
plt.ylabel('speedup, $t_1/t_P$')
plt.savefig('n-body-multiprocess-speedup.svg')

print(runtimes[0]/10.082616806030273)



