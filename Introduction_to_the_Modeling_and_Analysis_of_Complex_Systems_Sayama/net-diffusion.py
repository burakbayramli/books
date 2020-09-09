import matplotlib
matplotlib.use('TkAgg')
from pylab import *
import networkx as nx

def initialize():
    global g, nextg
    g = nx.karate_club_graph()
    g.pos = nx.spring_layout(g)
    for i in g.nodes_iter():
        g.node[i]['state'] = 1 if random() < .5 else 0
    nextg = g.copy()
    
def observe():
    global g, nextg
    cla()
    nx.draw(g, cmap = cm.binary, vmin = 0, vmax = 1,
            node_color = [g.node[i]['state'] for i in g.nodes_iter()],
            pos = g.pos)

alpha = 1 # diffusion constant
Dt = 0.01 # Delta t

def update():
    global g, nextg
    for i in g.nodes_iter():
        ci = g.node[i]['state']
        nextg.node[i]['state'] = ci + alpha * ( \
            sum(g.node[j]['state'] for j in g.neighbors(i)) \
            - ci * g.degree(i)) * Dt
    g, nextg = nextg, g

import pycxsimulator
pycxsimulator.GUI().start(func=[initialize, observe, update])
