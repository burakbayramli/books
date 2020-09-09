from pylab import *
import networkx as nx

g = nx.Graph()

imin, imax = 0, 9
for i in xrange(30):
    g.add_edge(randint(imin, imax),randint(imin, imax))

imin, imax = 10, 24
for i in xrange(40):
    g.add_edge(randint(imin, imax),randint(imin, imax))

imin, imax = 25, 49
for i in xrange(50):
    g.add_edge(randint(imin, imax),randint(imin, imax))

imin, imax = 0, 49
for i in xrange(10):
    g.add_edge(randint(imin, imax),randint(imin, imax))

nx.draw(g, pos = nx.spring_layout(g, k = 0.07, iterations = 300))
show()
