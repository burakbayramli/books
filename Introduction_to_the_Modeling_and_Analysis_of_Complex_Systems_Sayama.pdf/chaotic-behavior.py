from pylab import *

def initialize():
    global x, result
    x = 0.1
    result = [x]

def observe():
    global x, result
    result.append(x)

def update():
    global x, result
    x = x + r - x**2

r = 1.8
initialize()
for t in xrange(100):
    update()
    observe()
plot(result)
title('r = ' + str(r))
show()
