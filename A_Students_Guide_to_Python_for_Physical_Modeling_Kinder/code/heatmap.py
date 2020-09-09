# heatmap.py
# -------------------------------------------------------------------------
# Create two representations of a 2d histogram.
# ------------------------------------------------------------------------- 
import numpy as np, matplotlib.pyplot as plt
plt.figure()
x = np.random.randn(5000) - 1 # Generate random coordinates.
y = 2 * np.random.randn(5000)
# Create 2D histogram.  Same number of bins and same range for x and y.
counts, bins, _ = np.histogram2d(x, y, \
    bins=[100,100], range=[(-5,5), (-5,5)])

# Display data as a heatmap. Provide legend for colors.
plt.imshow(counts.transpose(), origin='lower', cmap='hot')
plt.colorbar()
plt.show()
