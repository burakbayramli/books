from pylab import *
import networkx as nx

g = nx.karate_club_graph()
positions = nx.spring_layout(g)

path = nx.shortest_path(g, 16, 25)
edges = [(path[i], path[i+1]) for i in xrange(len(path) - 1)]

nx.draw_networkx_edges(g, positions, edgelist = edges,
                       edge_color = 'r', width = 10)
nx.draw(g, positions, with_labels = True)
show()
