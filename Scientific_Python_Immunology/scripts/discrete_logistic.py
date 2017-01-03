import numpy
import pylab

def f(x, r):
    """Discrete logistic equation with parameter r"""
    return r*x*(1-x)

if __name__ == '__main__':
    # initial condition for x
    ys = []
    rs = numpy.linspace(0, 4, 400)
    for r in rs:
        x = 0.1
        for i in range(500):
            x = f(x, r)
        for i in range(50):
            x = f(x, r)
            ys.append([r, x])
            
    ys = numpy.array(ys)
    pylab.plot(ys[:,0], ys[:,1], '.')
    pylab.show()
