from pylab import *
import networkx as nx

g = nx.karate_club_graph()
positions = nx.spring_layout(g)

subplot(3, 2, 1)
nx.draw(g, positions, with_labels = True)
title('showing node names')

subplot(3, 2, 2)
nx.draw(g, positions, node_shape = '>')
title('using different node shape')

subplot(3, 2, 3)
nx.draw(g, positions, 
        node_size = [g.degree(i) * 50 for i in g.nodes()])
title('changing node sizes')

subplot(3, 2, 4)
nx.draw(g, positions, edge_color = 'pink',
        node_color = ['yellow' if i < 17 else 'green' for i in g.nodes()])
title('coloring nodes and edges')

subplot(3, 2, 5)
nx.draw_networkx_nodes(g, positions)
title('nodes only')

subplot(3, 2, 6)
nx.draw_networkx_edges(g, positions)
title('edges only')

show()
