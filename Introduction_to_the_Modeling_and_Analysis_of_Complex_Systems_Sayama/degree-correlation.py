from pylab import *
import networkx as nx

n = 1000
ba = nx.barabasi_albert_graph(n, 5)

xdata = []
ydata = []
for i, j in ba.edges_iter():
    xdata.append(ba.degree(i)); ydata.append(ba.degree(j))
    xdata.append(ba.degree(j)); ydata.append(ba.degree(i))

plot(xdata, ydata, 'o', alpha = 0.05)
show()
