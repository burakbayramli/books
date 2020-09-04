# waves.py
# -----------------------------------------------------------------------------
# Generate frames for an animation of a moving Gaussian waves.
# ----------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt
# get html_movie module from physicalmodelingwithpython.blogspot.com/
from html_movie import movie 

# Generate waves for each frame.
# Return a gaussian with specified center and spread using array s.
def gaussian(s, center=0.0, spread=1.0):
	return np.exp(-2 * (s - center)**2 / spread**2)

# All lengths are in [m], all times are in [s], and all speeds are in [m/s].
# Define the range of values to display.
xmin, xmax  = -4.0, 4.0
ymin, ymax  = -3.0, 3.0
# Define array of positions.
dx = 0.01
x = np.arange(xmin, xmax + dx, dx)

# Define the duration and number of frames for the simulation.
tmin, tmax  = 0.0, 4.0
num_frames = 100
t = np.linspace(tmin, tmax, num_frames)

# Define the initial position and speed of gaussian waves.
r_speed = 2.0		# speed of right-moving wave
r_0 = -4.0			# initial position of right-moving wave
l_speed = -2.0		# speed of left-moving wave
l_0 = 4.0			# initial position of left-moving wave

# Generate a figure and get access to its axes object.
plt.close('all')
fig = plt.figure(figsize=(10,10))
ax = plt.axes(xlim=(xmin, xmax), ylim=(ymin, ymax))

# Create three empty line objects and grab control.
# The loop below will update the lines in each frame.
ax.plot([], [], 'b--', lw=1)		# line for right-moving wave
ax.plot([], [], 'r--', lw=1)		# line for left-moving wave
ax.plot([], [], 'g-', lw=3)			# line for sum of waves
lines = ax.get_lines()				# get list of 3 line objects in plot

# It is essential that the frames be named in alphabetical order.
# {:03d} will display integers with three digits and insert zeros if needed:
# '000_movie.jpg', '001_movie.jpg', etc.
file_name = "{:03d}_movie.jpg"

# Generate frames and save each figure as a separate .jpg file.
for i in range(num_frames):
	r_now = r_0 + r_speed * t[i]		# update center of right-moving wave
	l_now = l_0 + l_speed * t[i]		# update center of left-moving wave
	yR =  gaussian(x, r_now)			# get current data for right-moving wave
	yL = -gaussian(x, l_now)			# get current data for left-moving wave
	lines[0].set_data(x,yR)				# update right-moving wave
	lines[1].set_data(x,yL)				# update left-moving wave
	lines[2].set_data(x,yR + yL)		# update sum of waves
	plt.savefig(file_name.format(i))	# save current plot

# Use html movie encoder adapted from scitools to create an HTML document that
# will display the frames as a movie.  Open movie.html in Web browser to view.
movie(input_files='*.jpg', output_file='movie.html')
