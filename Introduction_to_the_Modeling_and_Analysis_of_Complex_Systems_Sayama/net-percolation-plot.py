from pylab import *
import networkx as nx

p = 0.0001
pdata = []
gdata = []

while p < 0.1:
    pdata.append(p)
    g = nx.erdos_renyi_graph(100, p)
    ccs = nx.connected_components(g)
    gdata.append(max(len(cc) for cc in ccs))
    p *= 1.1

loglog(pdata, gdata)
xlabel('p')
ylabel('size of largest connected component')
show()
