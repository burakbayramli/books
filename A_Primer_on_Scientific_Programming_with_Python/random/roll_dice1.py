import random
import sys
N = int(sys.argv[1])  # no of experiments
M = 0                 # no of successful events
for i in range(N):
    six = 0           # count the no of dice with a six
    r1 = random.randint(1, 6)
    if r1 == 6:
        six += 1
    r2 = random.randint(1, 6)
    if r2 == 6:
        six += 1
    r3 = random.randint(1, 6)
    if r3 == 6:
        six += 1
    r4 = random.randint(1, 6)
    if r4 == 6:
        six += 1
    # Successful event?
    if six >= 2:
        M += 1
p = float(M)/N
print 'probability:', p

