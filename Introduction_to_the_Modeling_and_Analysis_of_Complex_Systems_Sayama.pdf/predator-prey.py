from pylab import *

def initialize():
    global x, y, xresult, yresult
    x = 1.
    y = 1.
    xresult = [x]
    yresult = [y]

def observe():
    global x, y, xresult, yresult
    xresult.append(x)
    yresult.append(y)

def update():
    global x, y, xresult, yresult
    r = 1.
    b = 1.
    d = 1.
    c = 1.
    K = 5.
    nextx = x + r * x * (1 - x / K) - (1 - 1 / (b * y + 1)) * x
    nexty = y - d * y + c * x * y
    x, y = nextx, nexty

initialize()
for t in xrange(100):
    update()
    observe()

plot(xresult, 'b-')
plot(yresult, 'g--')
show()
