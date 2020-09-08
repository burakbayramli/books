from pylab import *

r = 2.5
K = 1.

def initialize():
    global N, result
    N = 0.1
    result = [N]

def observe():
    global N, result
    result.append(N)

def f(N):
    return N + r * N * (1. - N / K)

def update():
    global N, result
    N = f(N)

initialize()
for t in xrange(30):
    update()
    observe()

Nmin, Nmax = 0, 1.4
plot([Nmin, Nmax], [Nmin, Nmax], 'k')

rng = arange(Nmin, Nmax, (Nmax - Nmin) / 100.)
plot(rng, map(f, rng), 'k')

horizontal = [result[0]]
vertical = [result[0]] 
for N in result[1:]:
    horizontal.append(vertical[-1])
    vertical.append(N)
    horizontal.append(N)
    vertical.append(N)    
plot(horizontal, vertical, 'b')

show()
