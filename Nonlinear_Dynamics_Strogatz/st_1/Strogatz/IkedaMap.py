import matplotlib.pyplot as plt
import numpy as np
import numba as nb
#numba gives a speedup of 3x for iterates 10^7...
#http://www.ams.org/samplings/feature-column/fcarc-henon
@nb.jit(nopython=True)
def IkedaMap(c,u, x,y):
    t = c-(6/(1+x**2+y**2))
    return 1+u*(x*np.cos(t)-y*np.sin(t)), u*(x*np.sin(t)+y*np.cos(t))

# Map dependent parameters
c =0.4
u = 0.9
iterates = 1e5

# Initial Condition
xtemp = 0.1
ytemp = 0.1

@nb.jit(nopython=True)
def make_points(xtemp=xtemp, ytemp=ytemp, iterates=iterates):
    x = [xtemp]
    y = [ytemp]
    for n in range(0,iterates):
        xtemp, ytemp = IkedaMap(c,u,xtemp,ytemp)
        x.append( xtemp )
        y.append( ytemp )
#        print('x:',xtemp,'y:',ytemp)
    return x,y
    
X,Y = make_points()

# Plot the time series
fig, ax = plt.subplots(figsize=(12,8))
ax.axis('equal')
# ax.set_axis_off()
ax.set_facecolor('y')
ax.set_title('Ikeda Plot where u = {0} and c= {1}.'.format(u,c))
ax.text(2,0.5,r'$t = c-\left(\frac{6}{(1+x**2+y**2)}\right)$')
ax.text(2,0,r'$x_{n + 1} = 1+u (x \cos(t) - y \sin(t))$')
ax.text(2,-0.5,r'$y_{n + 1} = 1+u (x \sin(t) + y \sin(t))$')
ax.plot(X,Y,'b,')
plt.show()