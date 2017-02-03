"""
As uniform_numbers1.py but the histogram is plotted both as a piecewise
constant curve and a piecewise linear curve. The number of bins is
read from the command line.
"""
import sys
N = int(sys.argv[1])
nbins = int(sys.argv[2])

import numpy as np
np.random.seed(12)
# Vectorized generation of random numbers
samples = np.random.random(size=N)

import scitools.std as st
x1, y1 = st.compute_histogram(samples, nbins, piecewise_constant=True)
x2, y2 = st.compute_histogram(samples, nbins, piecewise_constant=False)
st.plot(x1, y1, 'r', x2, y2, 'b')
st.title('%d samples of uniform numbers on (0,1)' % N)
st.hardcopy('tmp.eps')


