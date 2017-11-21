from mpl_toolkits.mplot3d import axes3d
import matplotlib.pyplot as plt
import numpy as np
from mayavi import mlab

def insect(r,k,x):
    return(r*(1-x/k)-x/(1+x**2))
mlab.figure(size=(900,800)) 
# mlab.axes() 
r,k,x = np.mgrid[-3:3:200j, -5:5:200j, -np.pi:np.pi:200j]
mobj = mlab.contour3d(r,k,x,insect)
boundaries = (-1,1,-5,5,-4,4)
mlab.axes(mobj,
                    extent=boundaries,
                    xlabel='r',
                    ylabel='k',
                    zlabel='x')
mlab.outline(mobj, extent=boundaries)
mlab.pipeline.vector_cut_plane(mobj, 
                                                scale_factor=1,
                                                opacity=0.4,
                                                transparent=True,
                                                plane_orientation='y_axes')
plt.show()