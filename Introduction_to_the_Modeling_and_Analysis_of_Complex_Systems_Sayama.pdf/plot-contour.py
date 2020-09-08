from pylab import *

xvalues, yvalues = meshgrid(arange(-5, 5.5, 0.05), arange(-5, 5.5, 0.05))
zvalues = sin(sqrt(xvalues**2 + yvalues**2))

cp = contour(xvalues, yvalues, zvalues)
clabel(cp)

show()
