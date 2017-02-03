import sys
import numpy as np
N = int(sys.argv[1])
x = np.zeros(N+1, np.int64)  # (long is equivalent to np.int64)
x[0] = 1
x[1] = 1
for n in range(2, N+1):
    x[n] = x[n-1] + x[n-2]
    print n, x[n]
