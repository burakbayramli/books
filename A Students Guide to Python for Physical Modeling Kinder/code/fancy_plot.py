# fancy_plot.py
# -------------------------------------------------------------------------
# Add a title and axis labels to a simple plot.
# ------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt

x_min, x_max = -4, 4
num_points = 51
x_list = np.linspace(x_min, x_max, num_points)
y_list = x_list**2

plt.figure()
plt.plot(x_list, y_list, 'r', linewidth=3)

ax = plt.gca()
ax.set_title("A Second Order Polynomial", fontsize=16)
ax.set_xlabel("$x$", fontsize=24)
ax.set_ylabel("$y = x^2$", fontsize=24)

plt.show()
