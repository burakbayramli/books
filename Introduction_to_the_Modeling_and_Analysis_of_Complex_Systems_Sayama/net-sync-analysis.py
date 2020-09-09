import matplotlib
matplotlib.use('TkAgg')
from pylab import *
import networkx as nx

def initialize():
    global g, nextg
    g = nx.karate_club_graph()
    g.pos = nx.spring_layout(g)
    for i in g.nodes_iter():
        g.node[i]['theta'] = random()
    nextg = g.copy()
    
def observe():
    global g, nextg
    subplot(1, 2, 1)
    cla()
    nx.draw(g, cmap = cm.hsv, vmin = -1, vmax = 1,
            node_color = [sin(g.node[i]['theta']) for i in g.nodes_iter()],
            pos = g.pos)
    axis('image')

    subplot(1, 2, 2)
    cla()
    plot([cos(g.node[i]['theta']) for i in g.nodes_iter()],
         [sin(g.node[i]['theta']) for i in g.nodes_iter()], '.')
    axis('image')
    axis([-1.1, 1.1, -1.1, 1.1])

alpha = 2 # coupling strength
beta  = 1 # acceleration rate
Dt = 0.001 # Delta t

def update():
    global g, nextg
    for i in g.nodes_iter():
        theta_i = g.node[i]['theta']
        nextg.node[i]['theta'] = theta_i + (beta * theta_i + alpha * \
            sum(g.node[j]['theta'] - theta_i for j in g.neighbors(i)) \
            ) * Dt
    g, nextg = nextg, g

import pycxsimulator
pycxsimulator.GUI().start(func=[initialize, observe, update])
