# Please attribute to Llewelyn Richards-Ward,
#llewelyn62@icloud.com
#Use and distribute as you want. 
from mayavi import mlab
import numpy as np
import matplotlib.pyplot as plt
import numba as nb
from scipy.integrate import odeint

#Create figure
mlab.figure(size=(900,800), bgcolor=(0,0,0), fgcolor=(1,1,1))
#For a very basic  approach which shows the steps this commented
#section can be used. eThe preferrend methods is to 
#use odeint and an array/function combination, as below. 
#Note the use of the Euler integration method.
#=========================================
#   # Integration time step
# dt = 0.001
#   # Lorenz ODE parameters
# sigma = 10.0
# r     = 28.0
# b     = 8.0/3.0
#   # Initial conditions
# x = -.5
# y =  .2
# z =  2.17
#   # Store the trajectory
# TrajX = []
# TrajY = []
# TrajZ = []
# Time  = []
# 
#   # Integrate the Lorenz ODEs
#   #Pretty basic approach -- in real examples would
#   #use odeint and an array/function combination. 
# for t in np.arange(0.,50.,dt):
# 	dxdt = sigma*(y - x)
# 	dydt = r * x - y - x * z
# 	dzdt = x * y - b * z
# 	x = x + dxdt * dt
# 	y = y + dydt * dt
# 	z = z + dzdt * dt
# 
# 	TrajX.append(x)
# 	TrajY.append(y)
# 	TrajZ.append(z)
# 	Time.append(t)
#=========================================

# Lorenz paramters and initial conditions
sigma, beta, rho = 10, 2, 28
u0, v0, w0 = 0, 1, 1.05

# Maximum time point and total number of time points
tmax, n = 50, 10000
@nb.jit(nopython=True) #optimises for speed. 
def deriv_lorenz(X, t, sigma, beta, rho):
    """The Lorenz equations."""
    x, y, z = X
    dx_dt = -sigma*(x - y)
    dy_dt = rho*x - y - x*z
    dz_dt = -beta*z + x*y
    return dx_dt, dy_dt, dz_dt

# Integrate the Lorenz equations on the time grid t
#Implements a detailed use of memory space for optimised accuracy, not
#so much speed. 
@nb.jit('float64(float64,float64,float64,float64,float64,float64,float64,float64)',nopython=False,parallel=True)
def int_f(u0=u0,v0=v0,w0=w0,sigma=sigma,beta=beta,rho=rho,tma=tmax,n=n):
    t = np.linspace(0, tmax, n)
    f = odeint(deriv_lorenz, (u0, v0, w0), t, args=(sigma, beta, rho))
    return f
x, y, z = int_f().T
Time = np.linspace(0,tmax,n)

mlab.plot3d(x,y,z,Time,colormap='Spectral',tube_radius=0.3)
mlab.outline(opacity=.4)
mlab.points3d(0,0,0,opacity=.5)
mlab.text3d(0,0,0,'Origin')
mlab.points3d(np.sqrt(beta*(rho-1)),np.sqrt(beta*(rho-1)),rho-1,opacity=.5)
mlab.text3d(np.sqrt(beta*(rho-1))*1.2,np.sqrt(beta*(rho-1)),rho-1,'C+')
mlab.points3d(-np.sqrt(beta*(rho-1)),-np.sqrt(beta*(rho-1)),rho-1,opacity=.5)
mlab.text3d(-np.sqrt(beta*(rho-1))*1.2,-np.sqrt(beta*(rho-1)),rho-1,'C-')
mlab.orientation_axes(xlabel='x',ylabel='y',zlabel='z')

#plt.show()
@mlab.animate(delay=50, ui=False)
def anim():
    f = mlab.gcf()
    while 1:
        f.scene.camera.azimuth(2)
        f.scene.render()
        yield

a = anim() # Starts the animation.
