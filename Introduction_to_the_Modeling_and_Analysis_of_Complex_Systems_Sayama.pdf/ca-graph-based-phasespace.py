from pylab import *
import networkx as nx

g = nx.DiGraph()

r = 2
L = 9

def config(x):
    return [1 if x & 2**i > 0 else 0 for i in range(L - 1, -1, -1)]

def cf_number(cf):
    return sum(cf[L - 1 - i] * 2**i for i in range(L))

def update(cf):
    nextcf = [0] * L
    for x in range(L):
        count = 0
        for dx in range(-r, r + 1):
            count += cf[(x + dx) % L]
        nextcf[x] = 1 if count > (2 * r + 1) * 0.5 else 0
    return nextcf

for x in xrange(2**L):
    g.add_edge(x, cf_number(update(config(x))))

ccs = [cc for cc in nx.connected_components(g.to_undirected())]
n = len(ccs)
w = ceil(sqrt(n))
h = ceil(n / w)
for i in xrange(n):
    subplot(h, w, i + 1)
    nx.draw(nx.subgraph(g, ccs[i]), with_labels = True)

show()
