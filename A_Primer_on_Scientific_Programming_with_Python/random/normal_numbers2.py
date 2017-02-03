# as normal_numbers1.py, but non-vectorized drawing of random numbers
import sys
N = int(sys.argv[1])
m = float(sys.argv[2])
s = float(sys.argv[3])

import numpy as np
import random
random.seed(12)
samples = [random.normalvariate(m, s) for i in range(N)]
print np.mean(samples), np.std(samples)

import scitools.std as st
x, y = st.compute_histogram(samples, 20, piecewise_constant=True)
st.plot(x, y, hardcopy='tmp.eps',
        title ='%d samples of Gaussian/normal numbers on (0,1)' % N)
