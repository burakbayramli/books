from pylab import *

xvalues, yvalues = meshgrid(arange(-3, 3.1, 0.1), arange(-3, 3.1, 0.1))

vx = 2 * xvalues
vy = yvalues - xvalues

streamplot(xvalues, yvalues, vx, vy)

show()
