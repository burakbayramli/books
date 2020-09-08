from pylab import *

def initialize(x0):
    global x, result
    x = x0
    result = [x]

def observe():
    global x, result
    result.append(x)

def update():
    global x, result
    x = x + r - x**2

r = 1.8
initialize(0.1)
for t in xrange(100):
    update()
    observe()
plot(result, 'b-')
initialize(0.100001)
for t in xrange(100):
    update()
    observe()
plot(result, 'g--')
title('r = ' + str(r))
show()
