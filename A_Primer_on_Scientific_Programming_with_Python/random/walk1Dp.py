from scitools.std import plot
import time, sys
import random, numpy

try:
    np = int(sys.argv[1])
    ns = int(sys.argv[2])
except IndexError:
    np = 4     # no of particles
    ns = 100   # no of steps


xmax = numpy.sqrt(ns); xmin = -xmax    # initial extent of plot axis
positions = numpy.zeros(np)            # all particles start at x=0
HEAD = 1;  TAIL = 2                    # constants

y = positions.copy()                   # y position is always 0

for step in range(ns):
    for p in range(np):
        coin = random.randint(1,2)     # flip coin
        if coin == HEAD:
            positions[p] += 1   # one unit length to the right
        elif coin == TAIL:
            positions[p] -= 1   # one unit length to the left

    # Extend x axis limits?
    if min(positions) < xmin:  xmin -= 2*numpy.sqrt(ns)
    if max(positions) > xmax:  xmax += 2*numpy.sqrt(ns)
    plot(positions, y, 'ko3', axis=[xmin, xmax, -0.2, 0.2])
    time.sleep(0.2)             # pause before next move
