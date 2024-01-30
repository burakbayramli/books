# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************
import numpy as np
from nusa import *
from nusa.mesh import *

m = Modeler()
m.add_rectangle((0,0),(0.3,0.3), esize=0.05)
nc, ec = m.generate_mesh()
x,y = nc[:,0], nc[:,1]

nodos = []
elementos = []

for k,nd in enumerate(nc):
    cn = Node((x[k],y[k]))
    nodos.append(cn)
    
for k,elm in enumerate(ec):
    i,j,m = int(elm[0]),int(elm[1]),int(elm[2])
    ni,nj,nm = nodos[i],nodos[j],nodos[m]
    ce = LinearTriangle((ni,nj,nm),200e9, 0.3, 0.01)
    elementos.append(ce)

m = LinearTriangleModel()
for node in nodos: m.add_node(node)
for elm in elementos: m.add_element(elm)

minx = min(x)
maxx = max(x)

nnf = len([node for node in nodos if node.x==maxx])
F = (6000./nnf)

for node in nodos:
    if node.x == minx:
        m.add_constraint(node, ux=0, uy=0)
    if node.x == maxx:
        m.add_force(node, (F,0))

m.plot_model()
m.solve()
# Plotting
m.plot_nsol("seqv") # von Mises Stress
#~ m.plot_nsol("exx")
m.show()


