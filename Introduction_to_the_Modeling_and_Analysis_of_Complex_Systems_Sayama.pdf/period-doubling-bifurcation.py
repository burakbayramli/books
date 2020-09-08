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

def plot_phase_space():
    initialize()
    for t in xrange(30):
        update()
        observe()
    plot(result)
    ylim(0, 2)
    title('r = ' + str(r))

rs = [0.1, 0.5, 1.0, 1.1, 1.5, 1.6]
for i in xrange(len(rs)):
    subplot(2, 3, i + 1)
    r = rs[i]
    plot_phase_space()

show()
