# graph_modifications.py
# -------------------------------------------------------------------------
# This script creates a simple plot with two lines, then modifies many
# features of the plot, including axis labels, labels and legend, line
# style, tick labels, and title.
# ------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt

# Generate data for plot.
num_points = 26
x_min, x_max = 0, 4
x_values = np.linspace(x_min, x_max, num_points)
y_values = x_values**2

# Create empty figure.
plt.figure()

# Plot data.
plt.plot(x_values, y_values, label="Population 1")		# label when plot is created
plt.plot(x_values, x_values**3, label="Population 2")	# label when plot is created
plt.legend()

# Gain control of current Axes object.
ax = plt.gca()

# Give plot a title.
ax.set_title("My First Plot", family='monospace', size=24, weight='bold')

# Label the axes.
ax.set_xlabel("Time [days]")
ax.set_ylabel("Population")

# Change tick labels and font
ax.set_xticklabels(ax.get_xticks(), family='monospace', fontsize=10)
ax.set_yticklabels(ax.get_yticks(), family='monospace', fontsize=10)

# Change the legend
lines = ax.get_lines()						# returns a list of line objects
lines[0].set_label("Infected Population")	# change labels using line objects
lines[1].set_label("Cured Population")		# change labels using line objects
ax.legend()									# display legend in plot

# Make the first line a thick, red, dashed line.
plt.setp(lines[0], linestyle='--', linewidth=3, color='r')

# Change the legend again.
ax.legend(("Healthy", "Recovered"))			# change labels using Axes object

plt.show()
