import random
import sys
import numpy as np

def roll_dice_py(N, ndice, nsix):
    M = 0            # no of successful events
    for i in range(N):
    	six = 0               # how many dice with six eyes?
    	for j in range(ndice):
            # Roll die no. j
            r = random.randint(1, 6)
            if r == 6:
               six += 1
        if six >= nsix:  # Successful event?
            M += 1
    p = float(M)/N
    return p

def roll_dice_vec1(N, ndice, nsix):
    eyes = np.random.random_integers(1, 6, (N, ndice))
    compare = eyes == 6
    nthrows_with_6 = np.sum(compare, axis=1)  # sum over columns
    nsuccesses = nthrows_with_6 >= nsix
    M = sum(nsuccesses)
    p = float(M)/N
    return p

def roll_dice_vec2(N, ndice, nsix):
    eyes = np.random.random_integers(1, 6, (N, ndice))
    six = [6 for i in range(ndice)]
    M = 0
    for i in range(N):
        # Check experiment no. i:
        compare = eyes[i,:] == six
        if np.sum(compare) == ndice:
            M += 1
    p = float(M)/N
    return p
    

from roll_dice_cy import roll_dice1 as roll_dice_cy1, \
     roll_dice2 as roll_dice_cy2

N = int(sys.argv[1])
ndice = 6
nsix =3

# Benchmark the various methods
import time
t0 = time.clock()
p = roll_dice_py(N, ndice, nsix)
t1 = time.clock()
print 'Loops in Python:                   ', t1-t0, p
t0 = time.clock()
p = roll_dice_vec1(N, ndice, nsix)
t1 = time.clock()
print 'Vectorized Python v1:              ', t1-t0, p
t0 = time.clock()
p = roll_dice_vec2(N, ndice, nsix)
t1 = time.clock()
print 'Vectorized Python v2:              ', t1-t0, p
t0 = time.clock()
p = roll_dice_cy1(N, ndice, nsix)
t1 = time.clock()
# Profiling of roll_dice_cy1
import cProfile, pstats
cProfile.runctx('roll_dice_cy1(N, ndice, nsix)', globals(), locals(), '.prof')
s = pstats.Stats('.prof')
s.strip_dirs().sort_stats('time').print_stats(30)

print 'Loops in Cython with random.random:', t1-t0, p
t0 = time.clock()
p = roll_dice_cy2(N, ndice, nsix)
t1 = time.clock()
print 'Loops in Cython with numpy.random: ', t1-t0, p

"""
Unix> python roll_dice.py 300000
Loops in Python:                    6.06 0.0617433333333
Vectorized Python v1:               1.54 0.06262
Vectorized Python v2:               9.72 1.33333333333e-05
Loops in Cython with random.random: 5.42 0.0618633333333
Loops in Cython with numpy.random:  0.07 0.0622866666667
"""
