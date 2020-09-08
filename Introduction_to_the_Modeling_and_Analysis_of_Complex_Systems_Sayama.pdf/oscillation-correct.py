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
    nextx = 0.5 * x + y
    nexty = -0.5 * x + y
    x, y = nextx, nexty

initialize()
for t in xrange(30):
    update()
    observe()

plot(xresult, 'b-')
plot(yresult, 'g--')
show()
