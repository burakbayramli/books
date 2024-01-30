# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************
from nusa import *

"""
Logan, D. (2007). A first course in the finite element analysis.
Example 3.1, pp. 70.
"""
# Input data 
E = 30e6 # psi
A = 2.0 # in^2
P = 10e3 # lbf
# Model
m = TrussModel("Truss Model")
# Nodes
n1 = Node((0,0))
n2 = Node((0,120))
n3 = Node((120,120))
n4 = Node((120,0))
# Elements
kdg = np.pi/180.0
e1 = Truss((n1,n2),E,A)
e2 = Truss((n1,n3),E,A)
e3 = Truss((n1,n4),E,A)

# Add elements 
for nd in (n1,n2,n3,n4):
    m.add_node(nd)
for el in (e1,e2,e3):
    m.add_element(el)

m.add_force(n1,(0,-P))
m.add_constraint(n2,ux=0,uy=0) # fixed 
m.add_constraint(n3,ux=0,uy=0) # fixed
m.add_constraint(n4,ux=0,uy=0) # fixed
m.plot_model()
m.solve() # Solve model
m.plot_deformed_shape() # plot deformed shape
m.show()

