# random_walk.py
# -------------------------------------------------------------------------
# Monte Carlo simulation of a two-dimensional random walk.
# ------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt
from numpy.random import random as rng

num_steps = 500
x_step = rng(num_steps) > 0.5
y_step = rng(num_steps) > 0.5
x_step = 2*x_step - 1
y_step = 2*y_step - 1
x_position = np.cumsum(x_step)
y_position = np.cumsum(y_step)

plt.figure()
plt.plot(x_position, y_position)
plt.axis('equal')

# A more succinct alternative:
x = ( 2*(rng(num_steps) > 0.5) - 1 ).cumsum()
y = ( 2*(rng(num_steps) > 0.5) - 1 ).cumsum()

plt.figure()
plt.plot(x,y)
plt.axis('equal')

plt.show()
