from pylab import *
import networkx as nx

g = nx.karate_club_graph()
n = g.number_of_nodes()
m = g.number_of_edges()
L = nx.average_shortest_path_length(g)
D = nx.diameter(g)
R = nx.radius(g)

Ldiffs = []
Ddiffs = []
Rdiffs = []

for i in xrange(500):
    g2 = nx.gnm_random_graph(n, m)
    if nx.is_connected(g2):
        Ldiffs.append(nx.average_shortest_path_length(g2) - L)
        Ddiffs.append(nx.diameter(g2) - D)
        Rdiffs.append(nx.radius(g2) - R)

subplot(1, 3, 1)
hist(Ldiffs)
title('L diff')

subplot(1, 3, 2)
hist(Ddiffs)
title('D diff')

subplot(1, 3, 3)
hist(Rdiffs)
title('R diff')

show()
