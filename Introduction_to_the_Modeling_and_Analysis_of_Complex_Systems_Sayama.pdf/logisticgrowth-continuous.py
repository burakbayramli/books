from pylab import *

r = 0.2
K = 1.0
Dt = 0.01

def initialize():
    global x, result, t, timesteps
    x = 0.1
    result = [x]
    t = 0.
    timesteps = [t]
    
def observe():
    global x, result, t, timesteps
    result.append(x)
    timesteps.append(t)

def update():
    global x, result, t, timesteps
    x = x + r * x * (1 - x / K) * Dt
    t = t + Dt

initialize()
while t < 50.:
    update()
    observe()

plot(timesteps, result)
show()
