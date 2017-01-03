import sys
import random
from beta import plot_beta

prob = float(sys.argv[1])
n = int(sys.argv[2])
alpha = float(sys.argv[3])
beta = float(sys.argv[4])

for i in xrange(1,n+1):
    if random.random() <= prob:
        alpha += 1
    else:
        beta += 1
    plot_beta(alpha,beta)
    
