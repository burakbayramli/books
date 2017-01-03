from gPy.Hypergraphs import Hypergraph

import fileinput

def process(line):
    edges = []
    line = line.rstrip()
    for edge in line.split(','):
        edges.append(edge)
    hg = Hypergraph(edges)
    for clique in hg.two_section().hypergraph():
        for hyperedge in hg:
            if clique <= hyperedge:
                break
        else:
            print '%s not graphical, because %s is a clique in its graph, but it is not contained in any hyperedge' % (hg,clique)
            return
    print '%s is graphical' % hg

for line in fileinput.input():
    process(line)
