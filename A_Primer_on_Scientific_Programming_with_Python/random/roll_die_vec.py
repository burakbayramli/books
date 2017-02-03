import numpy as np
import sys
N = int(sys.argv[1])
eyes = np.random.randint(1, 7, N)
success = eyes == 6     # True/False array
M = np.sum(success)     # treats True as 1, False as 0
print 'Got six %d times out of %d' % (M, N)
