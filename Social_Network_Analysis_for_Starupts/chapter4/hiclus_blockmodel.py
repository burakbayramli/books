__author__ = """\n""".join(['Maksim Tsvetovat <maksim@tsvetovat.org','Drew Conway <drew.conway@nyu.edu>',
                            'Aric Hagberg <hagberg@lanl.gov>'])

from collections import defaultdict
import networkx as nx
import numpy
from scipy.cluster import hierarchy
from scipy.spatial import distance
import matplotlib.pyplot as plt
import hc

"""Draw a blockmodel diagram of a clustering alongside the original network"""


def hiclus_blockmodel(G):
    # Extract largest connected component into graph H
    H=nx.connected_component_subgraphs(G)[0]
    # Create parititions with hierarchical clustering
    partitions=hc.create_hc(H)
    # Build blockmodel graph
    BM=nx.blockmodel(H,partitions)


    # Draw original graph
    pos=nx.spring_layout(H,iterations=100)
    fig=plt.figure(1,figsize=(6,10))
    ax=fig.add_subplot(211)
    nx.draw(H,pos,with_labels=False,node_size=10)
    plt.xlim(0,1)
    plt.ylim(0,1)

    # Draw block model with weighted edges and nodes sized by number of internal nodes
    node_size=[BM.node[x]['nnodes']*10 for x in BM.nodes()]
    edge_width=[(2*d['weight']) for (u,v,d) in BM.edges(data=True)]
    # Set positions to mean of positions of internal nodes from original graph
    posBM={}
    for n in BM:
        xy=numpy.array([pos[u] for u in BM.node[n]['graph']])
        posBM[n]=xy.mean(axis=0)
    ax=fig.add_subplot(212)
    nx.draw(BM,posBM,node_size=node_size,width=edge_width,with_labels=False)
    plt.xlim(0,1)
    plt.ylim(0,1)
    plt.axis('off')