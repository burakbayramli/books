import random
import sys
N = int(sys.argv[1])      # no of experiments
ndice = int(sys.argv[2])  # no of dice
nsix = int(sys.argv[3])   # wanted no of dice with six eyes
M = 0                     # no of successful events
for i in range(N):
    six = 0               # how many dice with six eyes?
    for j in range(ndice):
        # Roll die no. j
        r = random.randint(1, 6)
        if r == 6:
            six += 1
    # Successful event?
    if six >= nsix:
        M += 1
p = float(M)/N
print 'probability:', p

