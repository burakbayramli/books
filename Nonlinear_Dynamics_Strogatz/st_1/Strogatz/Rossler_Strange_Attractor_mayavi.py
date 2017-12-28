# Please attribute to Llewelyn Richards-Ward,
#llewelyn62@icloud.com
#Use and distribute as you want.
from mayavi import mlab
import numpy as np
import matplotlib.pyplot as plt
import numba as nb
from scipy.integrate import odeint

#Create figure
mlab.figure(size=(1200,1000), bgcolor=(0,0,0), fgcolor=(1,1,1))
# Lorenz paramters and initial conditions
a,b,c = .2,.2,5.7
u0, v0, w0 = 1,1,1

# Maximum time point and total number of time points
tmax, n = 250, 100000
#@nb.jit(nopython=False) #optimises for speed.
def deriv_rossler(X, t, a,b,c):
    """The Lorenz equations."""
    x, y, z = X
    dx_dt = -y-z
    dy_dt = x+a*y
    dz_dt = b +z*(x-c)
    return dx_dt, dy_dt, dz_dt

# Integrate the Lorenz equations on the time grid t
#Implements a detailed use of memory space for optimised accuracy, not
#so much speed.
#@nb.jit('float64(float64,float64,float64,float64,float64,float64,float64,float64)',nopython=False)
def int_f(u0=u0,v0=v0,w0=w0,a=a,b=b,c=c,tma=tmax,n=n):
    t = np.linspace(0, tmax, n)
    f = odeint(deriv_rossler, (u0, v0, w0), t, args=(a, b, c))
    return f
x, y, z = int_f().T
Time = np.linspace(0,tmax,n)

rossler = mlab.plot3d(x,y,z,Time,colormap='Spectral',tube_radius=0.1)
mlab.outline(opacity=.4)
mlab.points3d(0,0,0,opacity=.5)
mlab.text3d(0,0,0,'Origin')
mlab.points3d(u0,v0,w0,opacity=.5)
mlab.text3d(u0,v0,w0,'t_0')
mlab.orientation_axes(xlabel='x',ylabel='y',zlabel='z')
# mlab.pipeline.vector_cut_plane(rossler,
#                                                 scale_factor=.2,
#                                                 mode='2darrow',
#                                                 opacity=0.8,
#                                                 transparent=False,
#                                                 plane_orientation='y_axes')

plt.show()
# @mlab.animate(delay=50, ui=False)
# def anim():
#     f = mlab.gcf()
#     while 1:
#         f.scene.camera.azimuth(2)
#         f.scene.render()
#         yield

#a = anim() # Starts the animation.
