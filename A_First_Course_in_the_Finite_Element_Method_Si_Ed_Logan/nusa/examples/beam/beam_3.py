# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  License: MIT License
# ***********************************
import sys; sys.path.append('../..')
import numpy as np
from nusa.core import *
from nusa.model import *
from nusa.element import *

def test3():
    """
    Kattan, P. (XXXX).
    Example 7.1, pp. 109.
    """
    # Input data 
    E = 210e9
    I = 60e-6
    P = 20e3
    L = 2.0
    # Model
    m1 = BeamModel("Beam Model")
    # Nodes
    n1 = Node((0,0))
    n2 = Node((2,0))
    n3 = Node((4,0))
    # Elements
    e1 = Beam((n1,n2),E,I)
    e2 = Beam((n2,n3),E,I)

    # Add elements 
    for nd in (n1,n2,n3): m1.add_node(nd)
    for el in (e1,e2): m1.add_element(el)
        
    m1.add_force(n2, (-P,))
    m1.add_constraint(n1, ux=0, uy=0, ur=0) # fixed 
    m1.add_constraint(n3, ux=0, uy=0) # fixed
    m1.solve() # Solve model
    print(m1.KG)
    print(m1.U)


if __name__ == '__main__':
    test3()
