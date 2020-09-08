import matplotlib
matplotlib.use('TkAgg')
from pylab import *

n = 1000 # number of agents
r = 0.1 # neighborhood radius
th = 0.5 # threshold for moving

class agent:
    pass

def initialize():
    global agents
    agents = []
    for i in xrange(n):
        ag = agent()
        ag.type = randint(2)
        ag.x = random()
        ag.y = random()
        agents.append(ag)
    
def observe():
    global agents
    cla()
    white = [ag for ag in agents if ag.type == 0]
    black = [ag for ag in agents if ag.type == 1]
    plot([ag.x for ag in white], [ag.y for ag in white], 'wo')
    plot([ag.x for ag in black], [ag.y for ag in black], 'ko')
    axis('image')
    axis([0, 1, 0, 1])

def update():
    global agents
    ag = agents[randint(n)]
    neighbors = [nb for nb in agents
                 if (ag.x - nb.x)**2 + (ag.y - nb.y)**2 < r**2 and nb != ag]
    if len(neighbors) > 0:
        q = len([nb for nb in neighbors if nb.type == ag.type]) \
            / float(len(neighbors))
        if q < th:
            ag.x, ag.y = random(), random()

import pycxsimulator
pycxsimulator.GUI().start(func=[initialize, observe, update])
