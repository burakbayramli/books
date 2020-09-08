from pylab import *
import networkx as nx

g = nx.karate_club_graph()
nx.draw(g)
show()
