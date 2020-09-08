from pylab import *

Dt = 0.01

def initialize():
    global x, xresult, y, yresult
    x = y = 0.1
    xresult = [x]
    yresult = [y]
    
def observe():
    global x, xresult, y, yresult
    xresult.append(x)
    yresult.append(y)

def update():
    global x, xresult, y, yresult
    nextx = x + y * Dt
    nexty = y + (-r * (x**2 - 1) * y - x) * Dt
    x, y = nextx, nexty

def plot_phase_space():
    initialize()
    for t in xrange(10000):
        update()
        observe()
    plot(xresult, yresult)
    axis('image')
    axis([-3, 3, -3, 3])
    title('r = ' + str(r))

rs = [-1, -0.1, 0, .1, 1]
for i in xrange(len(rs)):
    subplot(1, len(rs), i + 1)
    r = rs[i]
    plot_phase_space()

show()
