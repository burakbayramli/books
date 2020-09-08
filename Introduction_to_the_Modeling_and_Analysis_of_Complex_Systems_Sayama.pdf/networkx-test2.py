import networkx as nx

# complete graph made of 5 nodes
g1 = nx.complete_graph(5)

# complete (fully connected) bipartite graph
# made of group of 3 nodes and group of 4 nodes
g2 = nx.complete_bipartite_graph(3, 4)

# Zachary's Karate Club graph
g3 = nx.karate_club_graph()
