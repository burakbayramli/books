from pylab import *
import networkx as nx

g = nx.read_adjlist('myNetworkData.csv', delimiter = ',',
                    create_using = nx.DiGraph())
nx.draw(g, with_labels = True)
show()
