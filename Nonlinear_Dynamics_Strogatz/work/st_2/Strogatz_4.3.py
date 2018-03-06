from mpl_toolkits.mplot3d import axes3d
import matplotlib.pyplot as plt
import numpy as np
from mayavi import mlab

def shm(w,A,O):
    return(w - A*np.sin(O))
mlab.figure(size=(900,800)) 
# mlab.axes() 
w,a,O = np.mgrid[-3:3:200j, -5:5:200j, -np.pi:np.pi:200j]
mobj = mlab.contour3d(w,a,O, shm)
boundaries = (-1,1,-5,5,-4,4)
mlab.axes(mobj,
                    extent=boundaries,
                    xlabel='Omega',
                    ylabel='a',
                    zlabel='Theta')
mlab.outline(mobj, extent=boundaries)
mlab.pipeline.vector_cut_plane(mobj, 
                                                scale_factor=1,
                                                opacity=0.4,
                                                transparent=False,
                                                plane_orientation='y_axes')
plt.show()