import matplotlib
matplotlib.use('TkAgg')
from pylab import *

n = 1000 # number of agents
w = 100 # number of rows/columns in spatial array

k = 1 # rate of cAMP decay
Dc = 0.001 # diffusion constant of cAMP
Dh = 0.01 # spatial resolution for cAMP simulation
Dt = 0.01 # time resolution for cAMP simulation

f = 1 # rate of cAMP secretion by an agent

class agent:
    pass

def initialize():
    global agents, env, nextenv

    agents = []
    for i in xrange(n):
        ag = agent()
        ag.x = randint(w)
        ag.y = randint(w)
        agents.append(ag)

    env = zeros([w, w])
    nextenv = zeros([w, w])

def observe():
    global agents, env, nextenv
    cla()
    imshow(env, cmap = cm.binary, vmin = 0, vmax = 1)
    axis('image')
    x = [ag.x for ag in agents]
    y = [ag.y for ag in agents]
    plot(y, x, 'b.') # x and y are swapped to match the orientation of env

def update():
    global agents, env, nextenv

    # simulating diffusion and evaporation of cAMP
    for x in xrange(w):
        for y in xrange(w):
            C, R, L, U, D = env[x,y], env[(x+1)%w,y], env[(x-1)%w,y], \
                            env[x,(y+1)%w], env[x,(y-1)%w]
            lap = (R + L + U + D - 4 * C)/(Dh**2)
            nextenv[x,y] = env[x,y] + (- k * C + Dc * lap) * Dt
    env, nextenv = nextenv, env

    # simulating secretion of cAMP by agents
    for ag in agents:
        env[ag.x, ag.y] += f * Dt

    # simulating chemotaxis of agents
    for ag in agents:
        newx, newy = (ag.x + randint(-1, 2)) % w, (ag.y + randint(-1, 2)) % w
        diff = (env[newx, newy] - env[ag.x, ag.y]) / 0.1
        if random() < exp(diff) / (1 + exp(diff)):
            ag.x, ag.y = newx, newy

import pycxsimulator
pycxsimulator.GUI().start(func=[initialize, observe, update])

