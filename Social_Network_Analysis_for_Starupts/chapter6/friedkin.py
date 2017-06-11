#!/usr/bin/env python
# encoding: utf-8
"""
friedkin.py

Created by Maksim Tsvetovat on 2011-08-08.
Copyright (c) 2011 __MyCompanyName__. All rights reserved.
"""

import sys
import os
import networkx as net
import matplotlib.pyplot as plot
import matplotlib.colors as colors
import random as r

class Person(object):
    
    def __init__(self, id):
        #Start with a single initial preference
        self.id=id
        self.i = r.random()
        self.a = self.i
        #we value initial opinion and subsequent information equally
        self.alpha=0.9
    
    def __str__(self):
        return(str(self.id))
        
    def step(self):
        #loop through the neighbors and aggregate their preferences
        neighbors=g[self]
        #all nodes in the list of neighbors are equally weighted, including self
        w=1/float((len(neighbors)+1))
        s=w*self.a
        for node in neighbors:
            s+=w*node.a

        # update my beliefs = initial belief plus sum of all influences
        self.a=(1-self.alpha)*self.i + self.alpha*s


class Influencer(Person):
    def __init__(self,id):
        self.id=id
        self.i = r.random()
        self.a = 1 ## opinion is strong and immovable
    
    def step(self):
        pass

density=0.6
g=net.Graph()

time=100

## create a network of Person objects
for i in range(10):
    p=Person(i)
    g.add_node(p)

##this will be a simple random graph
for x in g.nodes():
    for y in g.nodes():
        if r.random()<=density: g.add_edge(x,y)


influencers=4
connections=4
##add the influencers to the network and connect each to 3 other nodes
for i in range(influencers):
    inf=Influencer("Inf"+str(i))
    for x in range(connections):
        g.add_edge(r.choice(g.nodes()), inf)
    
            

col=[n.a for n in g.nodes()]
pos=net.spring_layout(g)
net.draw_networkx(g,pos=pos, node_color=col,cmap=plot.cm.Reds)
plot.figure(2)

for i in range(time):
    for node in g.nodes():
        node.step()
    
    col=[n.a for n in g.nodes()]  
    print col  
    plot.plot(col)    
plot.figure(i)
net.draw_networkx(g, pos=pos ,node_color=col, cmap=plot.cm.Reds)
    

