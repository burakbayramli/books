from pylab import *

a = b = c = d = 1.0
Dt = 0.01

def initialize():
    global x, xresult, y, yresult, t, timesteps
    x = y = 0.1
    xresult = [x]
    yresult = [y]
    t = 0.
    timesteps = [t]
    
def observe():
    global x, xresult, y, yresult, t, timesteps
    xresult.append(x)
    yresult.append(y)
    timesteps.append(t)

def update():
    global x, xresult, y, yresult, t, timesteps
    nextx = x + (a * x - b * x * y) * Dt
    nexty = y + (- c * y + d * x * y) * Dt
    x, y = nextx, nexty
    t = t + Dt

initialize()
while t < 50.:
    update()
    observe()

plot(timesteps, xresult, 'b')
plot(timesteps, yresult, 'g:')
show()
