from pylab import *

def initialize():
    global x, result
    x = 0.5
    result = [x]

def observe():
    global x, result
    result.append(x)

def f(x):
    return x**4 + 4*x**3*(1-x) + 4*x**2*(1-x)**2

def update():
    global x, result
    x = f(x)

initialize()
for t in xrange(30):
    update()
    observe()

### drawing diagonal line
xmin, xmax = 0, 1
plot([xmin, xmax], [xmin, xmax], 'k')

### drawing curve
rng = arange(xmin, xmax, (xmax - xmin) / 100.)
plot(rng, map(f, rng), 'k')

### drawing trajectory
horizontal = [result[0]]
vertical = [result[0]] 
for x in result[1:]:
    horizontal.append(vertical[-1])
    vertical.append(x)
    horizontal.append(x)
    vertical.append(x)    
plot(horizontal, vertical, 'b')

show()
