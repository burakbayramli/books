# solveODE.py
# -----------------------------------------------------------------------------
# Solution of ODE for harmonic oscillator.
# ----------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint

# Import function to integrate:
from simple_oscillator import F

# array of time values to study
t_min = 0; t_max = 10; dt = 0.1
t = np.arange(t_min, t_max+dt, dt)

# two sets of initial conditions
initial_conditions = [ (1.0, 0.0), (0.0, 1.0) ]

plt.figure()	# Create figure; then, add plots.
for y0 in initial_conditions:
	y = odeint(F, y0, t)
	plt.plot(t, y[:, 0], linewidth=2)

skip = 5
t_test = t[::skip]							# compare at a subset of points
plt.plot(t_test, np.cos(t_test), 'bo')		# exact solution for y0 = (1,0)
plt.plot(t_test, np.sin(t_test), 'go')		# exact solution for y0 = (0,1)

plt.show()
