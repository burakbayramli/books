# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************
import numpy as np
from nusa import *

n1 = Node((0,0))
n2 = Node((0.5,0))
n3 = Node((0.5,0.25))
n4 = Node((0,0.25))
n5 = Node((0,0.5))
e1 = LinearTriangle((n1,n3,n4),210e6,0.3,0.025)
e2 = LinearTriangle((n1,n2,n3),210e6,0.3,0.025)
e3 = LinearTriangle((n4,n3,n5),210e6,0.3,0.025)
m = LinearTriangleModel()
for node in (n1,n2,n3,n4,n5): m.add_node(node)
for elm in (e1,e2,e3): m.add_element(elm)
m.add_constraint(n1, ux=0, uy=0)
m.add_constraint(n4, ux=0, uy=0)
m.add_constraint(n5, ux=0, uy=0)
m.add_force(n2, (9375,0))
m.add_force(n3, (9375,0))
m.plot_model()
m.solve()

m.plot_nsol("seqv")
m.show()
