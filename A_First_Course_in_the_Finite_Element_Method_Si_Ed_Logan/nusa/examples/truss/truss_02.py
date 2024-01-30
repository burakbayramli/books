# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************
from nusa import *

"""
Kattan, P. (2003). MATLAB Guide to Finite Elements an interactive approach.
Problem 5.1, pp. 88.
"""
# Input data
E = 210e9
A = 0.005

n1 = Node((0,0))
n2 = Node((5,7))
n3 = Node((5,0))
n4 = Node((10,7))
n5 = Node((10,0))
n6 = Node((15,0))

e1 = Truss((n1,n2),E,A)
e2 = Truss((n1,n3),E,A)
e3 = Truss((n2,n3),E,A)
e4 = Truss((n2,n4),E,A)
e5 = Truss((n2,n5),E,A)
e6 = Truss((n3,n5),E,A)
e7 = Truss((n4,n5),E,A)
e8 = Truss((n4,n6),E,A)
e9 = Truss((n5,n6),E,A)

m = TrussModel("Example 02")

for node in (n1,n2,n3,n4,n5,n6): m.add_node(node)
for element in (e1,e2,e3,e4,e5,e6,e7,e8,e9): m.add_element(element)

m.add_constraint(n1, ux=0, uy=0)
m.add_constraint(n6, ux=0, uy=0)
m.add_force(n2, (20e3,0))
m.plot_model()
m.solve()
m.plot_deformed_shape()
m.show()










