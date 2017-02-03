import sys
N = int(sys.argv[1])

import numpy as np
np.random.seed(12)
# Vectorized generation of random numbers
samples = np.random.random(size=N)

# Plot histogram
import scitools.std as st
x, y = st.compute_histogram(samples, nbins=20)
st.plot(x, y, title='%d samples of uniform numbers on [0,1)' % N)
st.hardcopy('tmp.eps')


