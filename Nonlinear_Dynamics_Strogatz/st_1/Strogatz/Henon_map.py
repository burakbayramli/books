from matplotlib.pyplot import plot,show
import numba as nb
#numba gives a speedup of 3x for iterates 10^7...
#http://www.ams.org/samplings/feature-column/fcarc-henon
@nb.jit(nopython=True)
def HenonMap(a,b,x,y):
	return y + 1.0 - a *x*x, b * x

# Map dependent parameters
a =1.4# .2
b = 0.3#0.9991
iterates = 1e7

# Initial Condition
xtemp = 0.1
ytemp = 0.3

@nb.jit(nopython=True)
def make_points(xtemp=xtemp, ytemp=ytemp, iterates=iterates):
    x = [xtemp]
    y = [ytemp]
    for n in range(0,iterates):
        xtemp, ytemp = HenonMap(a,b,xtemp,ytemp)
        x.append( xtemp )
        y.append( ytemp )
#        print('x:',xtemp,'y:',ytemp)
    return x,y
X,Y = make_points()
# Plot the time series
plot(X,Y, 'g,')
show()
