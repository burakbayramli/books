from pylab import *

a = 1.1

def initialize():
    global x, result
    x = 1.
    result = [x]

def observe():
    global x, result
    result.append(x)

def f(x): ### iterative map is now defined as f(x)
    return a * x

def update():
    global x, result
    x = f(x)

initialize()
for t in xrange(30):
    update()
    observe()

### drawing diagonal line
xmin, xmax = 0, 20
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
