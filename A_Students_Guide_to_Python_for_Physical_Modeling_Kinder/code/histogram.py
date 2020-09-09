# histogram.py
# -------------------------------------------------------------------------
# Create histograms of random numbers.
# ------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt
from numpy.random import random as rng

#%%	Generate data and view PyPlot's default histogram.
data = rng(1000)

plt.figure()
plt.hist(data)

#%%	Get binned data from NumPy and make a colorful histogram where the
#	width of each bin is proportional to the number of elements in it.
counts, bin_edges = np.histogram(data)
bin_size = bin_edges[1] - bin_edges[0]
new_widths =  bin_size * counts / counts.max()

plt.figure()
plt.bar(bin_edges[:-1], counts, width=new_widths, color=['r','g','b'])

#%%	Provide logarithmically spaced bin edges rather than using defaults.
log2bins = np.logspace(-8, 0, num=9, base=2)
log2bins[0] = 0.0			# set first bin edge to zero instead of 1/256

plt.figure()
plt.hist(data, bins=log2bins)

plt.show()
