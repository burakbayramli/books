# perrin.py
# -------------------------------------------------------------------------
# Generate figure displaying Perrin's experimental data on Brownian motion.
# This script requires the data set 04brownian/g26perrindata.npy.
# ------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt

data = np.load('g26perrindata.npy')

plt.figure(figsize=(10,10))
plt.plot(data[:,0], data[:,1], 'b.', ms=16)
plt.axis([-20,20,-20,20])
plt.grid(b=True,which='major',ls='-',lw=1.5,c='g')
plt.xlabel('$\\Delta x$ [$\\mu$m]', fontsize=24)
plt.ylabel('$\\Delta y$ [$\\mu$m]', fontsize=24)
ax=plt.gca()
ax.set_xticklabels(ax.get_xticks(), weight='bold', fontsize=16)
ax.set_yticklabels(ax.get_yticks(), weight='bold', fontsize=16)
plt.show()
