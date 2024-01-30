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

def test2():
    """
    Logan, D. (2007). A first course in the finite element analysis.
    Example 4.4, pp. 171.
    """
    # Input data 
    E = 210e9
    I = 4e-4
    P = 10e3
    M = 20e3
    L = 3
    # Model
    m1 = BeamModel("Beam Model")
    # Nodes
    n1 = Node((0,0))
    n2 = Node((3,0))
    n3 = Node((6,0))
    # Elements
    e1 = Beam((n1,n2),E,I)
    e2 = Beam((n2,n3),E,I)

    # Add elements 
    for nd in (n1,n2,n3):
        m1.add_node(nd)
    for el in (e1,e2):
        m1.add_element(el)
        
    m1.add_force(n2, (-P,))
    m1.add_moment(n2, (M,))
    m1.add_constraint(n1, ux=0, uy=0, ur=0) # fixed 
    m1.add_constraint(n3, ux=0, uy=0, ur=0) # fixed
    m1.solve() # Solve model
    print(m1.KG)
    print(m1.U)
    print(m1.NF)


if __name__ == '__main__':
    test2()
