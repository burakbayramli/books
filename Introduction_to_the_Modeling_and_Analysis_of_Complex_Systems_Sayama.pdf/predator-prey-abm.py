import matplotlib
matplotlib.use('TkAgg')
from pylab import *
import copy as cp

nr = 500. # carrying capacity of rabbits

r_init = 100 # initial rabbit population
mr = 0.03 # magnitude of movement of rabbits
dr = 1.0 # death rate of rabbits when it faces foxes
rr = 0.1 # reproduction rate of rabbits

f_init = 30 # initial fox population
mf = 0.05 # magnitude of movement of foxes
df = 0.1 # death rate of foxes when there is no food
rf = 0.5 # reproduction rate of foxes

cd = 0.02 # radius for collision detection
cdsq = cd ** 2

class agent:
    pass

def initialize():
    global agents
    agents = []
    for i in xrange(r_init + f_init):
        ag = agent()
        ag.type = 'r' if i < r_init else 'f'
        ag.x = random()
        ag.y = random()
        agents.append(ag)

def observe():
    global agents
    cla()
    rabbits = [ag for ag in agents if ag.type == 'r']
    if len(rabbits) > 0:
        x = [ag.x for ag in rabbits]
        y = [ag.y for ag in rabbits]
        plot(x, y, 'b.')
    foxes = [ag for ag in agents if ag.type == 'f']
    if len(foxes) > 0:
        x = [ag.x for ag in foxes]
        y = [ag.y for ag in foxes]
        plot(x, y, 'ro')
    axis('image')
    axis([0, 1, 0, 1])

def update():
    global agents
    if agents == []:
        return

    ag = agents[randint(len(agents))]

    # simulating random movement
    m = mr if ag.type == 'r' else mf
    ag.x += uniform(-m, m)
    ag.y += uniform(-m, m)
    ag.x = 1 if ag.x > 1 else 0 if ag.x < 0 else ag.x
    ag.y = 1 if ag.y > 1 else 0 if ag.y < 0 else ag.y

    # detecting collision and simulating death or birth
    neighbors = [nb for nb in agents if nb.type != ag.type
                 and (ag.x - nb.x)**2 + (ag.y - nb.y)**2 < cdsq]

    if ag.type == 'r':
        if len(neighbors) > 0: # if there are foxes nearby
            if random() < dr:
                agents.remove(ag)
                return
        if random() < rr*(1-sum(1 for x in agents if x.type == 'r')/nr):
            agents.append(cp.copy(ag))
    else:
        if len(neighbors) == 0: # if there are no rabbits nearby
            if random() < df:
                agents.remove(ag)
                return
        else: # if there are rabbits nearby
            if random() < rf:
                agents.append(cp.copy(ag))

def update_one_unit_time():
    global agents
    t = 0.
    while t < 1. and len(agents) > 0:
        t += 1. / len(agents)
        update()

import pycxsimulator
pycxsimulator.GUI().start(func=[initialize, observe, update_one_unit_time])
