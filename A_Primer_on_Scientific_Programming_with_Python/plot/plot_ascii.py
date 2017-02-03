"""Make plot of a curve in pure text format (ASCII)."""
# This is the same example as in the scitools.aplotter doc string

import numpy as np
x = np.linspace(-2, 2, 81)
y = np.exp(-0.5*x**2)*np.cos(np.pi*x)
from scitools.aplotter import plot
plot(x, y)

plot(x, y, draw_axes=False)

# Plot symbols (the dot argument) at data points
plot(x, y, plot_slope=False)
 
# Drop axis labels
plot(x, y, plot_labels=False)

plot(x, y, dot='o', plot_slope=False)

# Store plot in a string
p = plot(x, y, output=str)
print p



 

