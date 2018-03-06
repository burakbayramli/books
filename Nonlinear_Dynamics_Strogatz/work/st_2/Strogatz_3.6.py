from mpl_toolkits.mplot3d import axes3d
import matplotlib.pyplot as plt
import numpy as np
from mayavi import mlab

def cusp_bi(x,r,h):
    return(h - r*x - x**3)
mlab.figure(size=(800,700)) 
# mlab.axes() 
x,r , h = np.mgrid[-20:20:200j, -40:40:200j, -40:40:200j]
cusp = mlab.contour3d(x,r,h, cusp_bi,transparent=True)
boundaries = (-15,15,-40,40,-40,40)
mlab.axes(cusp,
                    extent=boundaries)
mlab.outline(cusp, extent=boundaries)
mlab.pipeline.vector_cut_plane(cusp, 
                                                scale_factor=1,
                                                mode='2darrow',
                                                opacity=0.8,
                                                transparent=False,
                                                plane_orientation='y_axes')
plt.show()