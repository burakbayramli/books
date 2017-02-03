"""
Vectorized implementation of walk1Dp.py/walk1Ds.py,
but without graphics at each step (for efficiency).
"""
import numpy  # not as np since np is an important variable here
import time, sys
from scitools.std import plot, hardcopy, compute_histogram

numpy.random.seed(11)

try:
    np = int(sys.argv[1])  # number of particles
    ns = int(sys.argv[2])  # number of steps
except IndexError:
    np = 2000
    ns = 200

# Draw from 1, 2
moves = numpy.random.random_integers(1, 2, size=np*ns)
# Transform 1 to -1 and 2 to 1
moves = 2*moves - 3
moves.shape = (ns, np)

positions = numpy.zeros(np)
for step in range(ns):
    positions += moves[step, :]
    
    mean_pos = numpy.mean(positions)
    stdev_pos = numpy.std(positions)
    print mean_pos, stdev_pos

nbins = int(3*numpy.sqrt(ns))    # no of intervals in histogram
pos, freq = compute_histogram(positions, nbins,
                              piecewise_constant=True)

plot(positions, numpy.zeros(np), 'ko3',
     pos, freq, 'r', 
     axis=[min(positions), max(positions), -0.01, 1.1*max(freq)],
     hardcopy='tmp.eps')

