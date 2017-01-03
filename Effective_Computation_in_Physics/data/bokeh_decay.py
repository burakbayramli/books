import numpy as np
# import the Bokeh plotting tools
from bokeh import plotting as bp

# as in the above example, load decays.csv into a numpy array
decaydata = np.loadtxt('decays2.csv',delimiter=",",skiprows=1)

# provide handles for the x and y columns
time = decaydata[:,0]
decays = decaydata[:,1]

bp.output_file("decays.html", title="bokeh_decay.py example")

bp.figure(tools="pan,wheel_zoom,box_zoom,reset,previewsave")

bp.line(time, decays, x_axis_label="Time (s)", y_axis_label="Decays (#)",
     color='#1F78B4', legend='Decays per second')

bp.curplot().title = "Decays"
bp.grid().grid_line_alpha=0.3

bp.show()  # open a browser
