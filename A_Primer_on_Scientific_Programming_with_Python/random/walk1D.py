import random
import numpy
np = 4                          # no of particles
ns = 100                        # no of steps
positions = numpy.zeros(np)     # all particles start at x=0
HEAD = 1;  TAIL = 2             # constants

for step in range(ns):
    for p in range(np):
        coin = random.randint(1,2)  # flip coin
        if coin == HEAD:
            positions[p] += 1   # one unit length to the right
        elif coin == TAIL:
            positions[p] -= 1   # one unit length to the left
