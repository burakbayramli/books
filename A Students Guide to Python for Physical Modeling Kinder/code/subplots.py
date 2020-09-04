# subplots.py
# -------------------------------------------------------------------------
# Create four plots in the same figure.
# ------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt
from numpy.random import random

t = np.linspace(0, 1, 101)

plt.figure()
plt.subplot(2, 2, 1); plt.hist(random(20))
plt.subplot(2, 2, 2); plt.plot(t, t**2, t, t**3 - t)
plt.subplot(2, 2, 3); plt.plot(random(20), random(20), 'r*')
plt.subplot(2, 2, 4); plt.plot(t*np.cos(10*t), t*np.sin(10*t))
plt.show()
