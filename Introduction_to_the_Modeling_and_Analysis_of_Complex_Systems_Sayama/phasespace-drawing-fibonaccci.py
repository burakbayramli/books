from pylab import *

def initialize(x0, y0): ###
    global x, y, xresult, yresult
    x = x0 ###
    y = y0 ###
    xresult = [x]
    yresult = [y]

def observe():
    global x, y, xresult, yresult
    xresult.append(x)
    yresult.append(y)

def update():
    global x, y, xresult, yresult
    nextx = x + y
    nexty = x
    x, y = nextx, nexty

for x0 in arange(-2, 2.5, .5): ###
    for y0 in arange(-2, 2.5, .5): ###
        initialize(x0, y0) ###
        for t in xrange(3):
            update()
            observe()
        plot(xresult, yresult, 'b') ###

show()
