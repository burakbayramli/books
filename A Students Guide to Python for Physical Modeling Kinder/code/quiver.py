# quiver.py  
# -------------------------------------------------------------------------
# Create a quiver plot.
# ------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt

coords = np.linspace(-1, 1, 11)
X, Y = np.meshgrid(coords, coords)
Vx, Vy = Y, -X

plt.figure()
plt.quiver(X, Y, Vx, Vy)
plt.show()
