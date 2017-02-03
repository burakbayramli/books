# Make a figure of a curve and "dart throws" for illustrating
# Monte Carlo integration in 2D for area computations.
import numpy as np
xr = np.random.uniform(0, 2, 500)
yr = np.random.uniform(0, 2.4, 500)
x = np.linspace(0, 2, 51)
from scitools.std import exp, sin, pi, plot
y = 2 + x**2*exp(-0.5*x)*sin(pi*x)
plot(x, y, 'r', xr, yr, 'o', hardcopy='tmp.eps')
