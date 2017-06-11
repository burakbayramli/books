#!/usr/bin/env python
# encoding: utf-8
"""
hijackers.py

Created by Maksim Tsvetovat on 2011-08-16.
Copyright (c) 2011 Maksim Tsvetovat. All rights reserved.
"""

import sys
import os

import csv ## we'll use the built-in CSV library
import networkx as net
import multimode as mm
import triadic

# open the file
in_file=csv.reader(open('9_11_edgelist.txt','rb'))

g=net.Graph()
for line in in_file:
    g.add_edge(line[0],line[1],weight=line[2],conf=line[3])
    

#first, let's make sure that all nodes in the graph have the 'flight' attribute
for n in g.nodes_iter(): g.node[n]['flight']='None'

attrb=csv.reader(open('9_11_attrib.txt','rb'))
for line in attrb:
    g.node[line[0]]['flight']=line[1]


# Connected_component_subgraphs() returns a list of components, sorted largest to smallest
components=net.connected_component_subgraphs(g)

# pick the first and largest component
cc = components[0]

# type-string tells the function what attribute to differentiate on
mm.plot_multimode(cc,type_string='flight')

# run triadic analysis
census, node_census = triadic.triadic_census(cc2)