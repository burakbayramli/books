# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  License: MIT License
# ***********************************
import numpy as np
from nusa.core import *
from nusa.model import *
from nusa.element import *

def test4():
    """
    Beer & Johnston. Mechanics of materials
    Problem 9.13 , pp. 568.
    """
    # Input data 
    E = 29e6
    I = 291 # W14x30 
    P = 35e3
    L1 = 5*12 # in
    L2 = 10*12 #in
    # Model
    m1 = BeamModel("Beam Model")
    # Nodes
    n1 = Node((0,0))
    n2 = Node((L1,0))
    n3 = Node((L1+L2,0))
    # Elements
    e1 = Beam((n1,n2),E,I)
    e2 = Beam((n2,n3),E,I)

    # Add elements 
    for nd in (n1,n2,n3): m1.add_node(nd)
    for el in (e1,e2): m1.add_element(el)
        
    m1.add_force(n2, (-P,))
    m1.add_constraint(n1, ux=0, uy=0) # fixed 
    m1.add_constraint(n3, uy=0) # fixed
    m1.solve() # Solve model
    # Displacement at C point
    print("Displacement at point C: {0}".format(n2.uy))
    m1.plot_moment_diagram()
    m1.plot_shear_diagram()
    m1.show()


if __name__ == '__main__':
    test4()
