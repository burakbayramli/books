#!/usr/bin/env python
# encoding: utf-8
"""
two_mode.py

Created by Maksim Tsvetovat on 2011-08-17.
Copyright (c) 2011 __MyCompanyName__. All rights reserved.
"""

import sys
import os
import csv 
import math
import networkx as net
import matplotlib.pyplot as plot

## Import bi-partite (bi-modal) functions
from networkx.algorithms import bipartite as bi

def trim_edges(g, weight=1):
	g2=net.Graph()
	for f, to, edata in g.edges(data=True):
		if edata['weight'] > weight:
			g2.add_edge(f,to,edata)
	return g2


## Read the data from a CSV file
## We use the Universal new-line mode since many CSV files are created with Excel
r=csv.reader(open('campaign_short.csv','rU'))

## 2-mode graphs are usually directed. Here, their direction implies money flow
g=net.Graph()

## we need to keep track separately of nodes of all types
pacs=[]
candidates=[]

## Construct a directed graph from edges in the CSV file
for row in r: 
    if row[0] not in pacs: 
        pacs.append(row[0])
    if row[12] not in candidates: 
        candidates.append(row[12])
    g.add_edge(row[0],row[12], weight=int(row[10]))
    
## compute the projected graph
pacnet=bi.weighted_projected_graph(g, pacs, ratio=False)
pacnet=net.connected_component_subgraphs(pacnet)[0]
weights=[math.log(edata['weight']) for f,t,edata in pacnet.edges(data=True)]

net.draw_networkx(p,width=weights, edge_color=weights)



## Compute the candidate network
cannet=bi.weighted_projected_graph(g, candidates, ratio=False)
cannet=net.connected_component_subgraphs(cannet)[0]
weights=[math.log(edata['weight']) for f,t,edata in cannet.edges(data=True)]
plot.figure(2) ## switch to a fresh canvas
net.draw_networkx(cannet,width=weights, edge_color=weights)


plot.figure(3)
plot.hist(weights)

## The weights histogram is logarithmic; we should compute the original weight = e^log_weight
cannet_trim=trim_edges(cannet, weight=math.exp(0.9))

plot.figure(4)
## re-calculate weights based on the new graph
weights=[edata['weight'] for f,t,edata in cannet_trim.edges(data=True)]
net.draw_networkx(cannet_trim,width=weights, edge_color=weights)