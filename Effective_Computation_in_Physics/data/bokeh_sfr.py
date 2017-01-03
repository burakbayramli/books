import numpy as np
# import the Bokeh plotting tools
from bokeh import plotting as bp

# as in the above example, load deploys.csv into a numpy array
deploydata = np.loadtxt('sfr_dep.dat',delimiter=" ")

# provide handles for the x and y columns
time = deploydata[:,0]
deploys = deploydata[:,1]

lwrs = []
for v in deploydata[:,1] : 
  lwrs.append(100-int(v/3))

bp.output_file("sfr.html", title="bokeh_sfr.py example")

bp.figure(tools="pan,wheel_zoom,box_zoom,reset,previewsave")

bp.line(time, deploys, x_axis_label="Time (m)", y_axis_label="Reactors (#)",
     color='#1F78B4', legend='SFRs')

bp.hold()

bp.line(time, lwrs, x_axis_label="Time (m)", y_axis_label="Reactors (#)",
     color="green", legend='LWRs')

bp.curplot().title = "SFRs"
bp.grid().grid_line_alpha=0.3

bp.show()  # open a browser
