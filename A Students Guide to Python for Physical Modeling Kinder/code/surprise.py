# surprise.py
# -------------------------------------------------------------------------
# This script will create a familar but interesting image.
# It may take about a minute to run.
# ------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt

# This determines the number of colors used in the plot.
# The larger the value, the longer the script will take.
max_iterations = 32

# These parameters define the boundaries of the plot
x_min, x_max = -2.5, 1.5
y_min, y_max = -1.5, 1.5

# This parameter controls the grid spacing.  A smaller value gives better
# resolution, but the script will take longer to run.
ds = 0.002

X = np.arange(x_min, x_max + ds, ds)
Y = np.arange(y_min, y_max + ds, ds)
data = np.zeros( (X.size, Y.size), dtype='uint')

for i in range(X.size):
	for j in range(Y.size):
		x0, y0 = X[i], Y[j]
		x, y = x0, y0
		count = 0
		while count < max_iterations:
			# Update x and y simultaneously.
			x, y = (x0 + x*x - y*y, y0 + 2*x*y)
			# Exit loop if (x,y) is too far from the origin.
			if (x*x + y*y) > 4.0: break
			count += 1
		data[i, j] = max_iterations - count

plt.imshow(data.transpose(), interpolation='nearest', cmap='jet')
plt.axis('off')
plt.show()
