# contour.py
# -------------------------------------------------------------------------
# Create a labeled contour plot.
# ------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt

#%% Create a grid of x and y coordinates
x_vals = np.linspace(-3, 3, 21)
y_vals = np.linspace(0, 10, 11)
X, Y = np.meshgrid(x_vals, y_vals)

#%% Generate function values. 
Z = np.cos(X) * np.sin(Y)

#%% Plot and label contours.
plt.figure()
cs = plt.contour(X, Y, Z, 10, linewidths=3, colors='k')
plt.clabel(cs,fontsize=10)
plt.show()
