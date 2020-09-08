import matplotlib
matplotlib.use('TkAgg')
from pylab import *
import networkx as nx
import random as rd

def initialize():
    global g, nextg
    g = nx.erdos_renyi_graph(100, 0.1)
    g.pos = nx.spring_layout(g)
    for i in g.nodes_iter():
        g.node[i]['state'] = 1 if random() < .5 else 0
    nextg = g.copy()
    
def observe():
    global g
    cla()
    nx.draw(g, cmap = cm.binary, vmin = 0, vmax = 1,
            node_color = [g.node[i]['state'] for i in g.nodes_iter()],
            pos = g.pos)

p_i = 0.1 # infection probability
p_r = 0.5 # recovery probability

def update():
    global g, nextg
    for a in g.nodes_iter():
        if g.node[a]['state'] == 0: # if susceptible
            nextg.node[a]['state'] = 0
            for b in g.neighbors(a):
                if g.node[b]['state'] == 1: # if neighbor b is infected
                    if random() < p_i:
                        nextg.node[a]['state'] = 1
        else: # if infected
            nextg.node[a]['state'] = 0 if random() < p_r else 1
    nextg, g = g, nextg

import pycxsimulator
pycxsimulator.GUI().start(func=[initialize, observe, update])
