from pylab import *
import networkx as nx

subplot(2, 2, 1)
nx.draw(nx.gnm_random_graph(10, 20))
title('random graph with\n10 nodes, 20 edges')

subplot(2, 2, 2)
nx.draw(nx.gnp_random_graph(20, 0.1))
title('random graph with\n 20 nodes, 10% edge probability')

subplot(2, 2, 3)
nx.draw(nx.random_regular_graph(3, 10))
title('random regular graph with\n10 nodes of degree 3')

subplot(2, 2, 4)
nx.draw(nx.random_degree_sequence_graph([3,3,3,3,4,4,4,4,5,5]))
title('random graph with\n degree sequence\n[3,3,3,3,4,4,4,4,5,5]')

show()
