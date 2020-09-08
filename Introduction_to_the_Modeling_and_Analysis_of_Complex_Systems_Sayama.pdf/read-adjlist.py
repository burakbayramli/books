from pylab import *
import networkx as nx

g = nx.read_adjlist('myNetworkData.csv', delimiter = ',')
nx.draw(g, with_labels = True)
show()
