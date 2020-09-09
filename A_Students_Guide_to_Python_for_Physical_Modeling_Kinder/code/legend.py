# legend.py
# -------------------------------------------------------------------------
# Create a plot with a legend to distinguish multiple curves.
# ------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt

num_curves = 3
x = np.linspace(0, 1, 51)
y = np.zeros((x.size, num_curves))
for n in range(num_curves):
	y[:, n] = np.sin((n+1) * x * 2 * np.pi)

plt.figure()
plt.plot(x, y, linewidth=2)

ax = plt.gca()
ax.legend(	("sin(2$\\pi$x)", "sin(4$\\pi$x)", "sin(6$\\pi$x)") )

plt.show()
