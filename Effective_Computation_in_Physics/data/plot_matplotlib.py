import numpy as np
# import the Bokeh plotting tools
from bokeh import plotting as bp

# as in the above example, load decays.csv into a numpy array
decaydata = np.loadtxt('decays.csv',delimiter=",",skiprows=1)

# provide handles for the x and y columns
time = decaydata[:,0]
decays = decaydata[:,1]

# import the matplotlib plotting functionality
import pylab as plt

plt.plot(time, decays)

plt.xlabel('Time (s)')
plt.ylabel('Decays')
plt.title('Decays')
plt.grid(True)
plt.savefig("decays_matplotlib.png")
