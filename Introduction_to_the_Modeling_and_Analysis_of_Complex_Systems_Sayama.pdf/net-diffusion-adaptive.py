import matplotlib
matplotlib.use('TkAgg')
from pylab import *
import networkx as nx

def initialize():
    global g, nextg
    g = nx.karate_club_graph()
    for i, j in g.edges_iter():
        g.edge[i][j]['weight'] = 0.5
    g.pos = nx.spring_layout(g)
    for i in g.nodes_iter():
        g.node[i]['state'] = 1 if g.node[i]['club'] == 'Mr. Hi' else 0
    nextg = g.copy()
    
def observe():
    global g, nextg
    cla()
    nx.draw(g, cmap = cm.binary, vmin = 0, vmax = 1,
            node_color = [g.node[i]['state'] for i in g.nodes_iter()],
            edge_cmap = cm.binary, edge_vmin = 0, edge_vmax = 1,
            edge_color = [g.edge[i][j]['weight'] for i, j in g.edges_iter()],
            pos = g.pos)

alpha = 1 # diffusion constant
beta =  3 # rate of adaptive edge weight change
gamma = 3 # pickiness of nodes
Dt = 0.01 # Delta t

def update():
    global g, nextg
    for i in g.nodes_iter():
        ci = g.node[i]['state']
        nextg.node[i]['state'] = ci + alpha * ( \
            sum((g.node[j]['state'] - ci) * g.edge[i][j]['weight']
                for j in g.neighbors(i))) * Dt
    for i, j in g.edges_iter():
        wij = g.edge[i][j]['weight']
        nextg.edge[i][j]['weight'] = wij + beta * wij * (1 - wij) * ( \
            1 - gamma * abs(g.node[i]['state'] - g.node[j]['state'])
            ) * Dt
    nextg.pos = nx.spring_layout(nextg, pos = g.pos, iterations = 5)
    g, nextg = nextg, g

import pycxsimulator
pycxsimulator.GUI().start(func=[initialize, observe, update])
