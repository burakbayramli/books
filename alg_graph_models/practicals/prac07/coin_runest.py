import sys
import random

prob = float(sys.argv[1])
n = int(sys.argv[2])
hcount = 0.0

for i in xrange(1,n+1):
    if random.random() <= prob:
        hcount += 1
    print hcount/i
    
