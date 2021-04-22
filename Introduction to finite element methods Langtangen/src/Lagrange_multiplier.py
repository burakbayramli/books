from numpy import *
from pylab import *


x = arange(-2, 2, 0.03)

f = x**2 
g = x + 1


plot(x, f) 
plot(x, g)
legend(["f=x^2", "g=x-1"], loc="lower right")
show()


