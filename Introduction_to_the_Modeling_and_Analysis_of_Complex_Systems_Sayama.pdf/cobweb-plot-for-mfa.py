from pylab import *

def initialize():
    global x, result
    x = 0.4
    result = [x]

def observe():
    global x, result
    result.append(x)

def f(x): 
    return 70*x**9 - 315*x**8 + 540*x**7 - 420*x**6 + 126*x**5

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
