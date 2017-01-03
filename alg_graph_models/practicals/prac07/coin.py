import sys
import random

prob = float(sys.argv[1])
n = int(sys.argv[2])

for i in xrange(n):
    if random.random() > prob:
        print 'T'
    else:
        print 'H'
    
