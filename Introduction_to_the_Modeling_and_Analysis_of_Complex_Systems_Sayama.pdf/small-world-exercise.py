import matplotlib
matplotlib.use('TkAgg')
from pylab import *
import networkx as nx
import random as rd

n = 30 # number of nodes
k = 4  # number of neighbors of each node

def initialize():
    global g
    g = nx.grid_2d_graph(10, 10)
    g.pos = nx.spring_layout(g, k = 0.03)
    g.count = 0
    
def observe():
    global g
    cla()
    nx.draw(g, pos = g.pos)

def update():
    global g
    g.count += 1
    if g.count % 20 == 0: # rewiring once in every 20 steps
        nds = g.nodes()
        i = rd.choice(nds)
        if g.degree(i) > 0:
            g.remove_edge(i, rd.choice(g.neighbors(i)))
            nds.remove(i)
            for j in g.neighbors(i):
                nds.remove(j)
            g.add_edge(i, rd.choice(nds))

    # simulation of node movement
    g.pos = nx.spring_layout(g, pos = g.pos, iterations = 5, k = 0.03)

import pycxsimulator
pycxsimulator.GUI().start(func=[initialize, observe, update])
