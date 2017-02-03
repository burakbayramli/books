from integrate import *
from scitools.std import *


i = 1
symbols = ['o', '+', '*', '^']
a = 0; b = 10
n = 11
for Method in Midpoint, Trapezoidal, Simpson, GaussLegendre2:
    m = Method(a, b, n)
    v = m.integrate(lambda x: x)  # dummy func
    print m.__class__.__name__
    x = m.points
    print x
    y = [i]*len(x)
    plot(x, y, symbols[i-1])
    legend(m.__class__.__name__)
    hold('n')
    i += 1
    axis([a-1, b+1, 0, i])
plot([a, a], [0, i], '.')
plot([b, b], [0, i], '.')
hardcopy('tmp.eps')

    
