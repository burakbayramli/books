# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  License: MIT License
# ***********************************

from nusa.core import *
from nusa.model import *
from nusa.element import *
from nusa.graph import *

def test1():
    """
    Logan, D. (2007). A first course in the finite element analysis.
    Example 2.1, pp. 42.
    """
    P = 5000.0
    k1, k2, k3 = 1000, 2000, 3000
    # Model
    m1 = SpringModel("2D Model")
    # Nodes
    n1 = Node((0,0))
    n2 = Node((0,0))
    n3 = Node((0,0))
    n4 = Node((0,0))
    # Elements
    e1 = Spring((n1,n3),k1)
    e2 = Spring((n3,n4),k2)
    e3 = Spring((n4,n2),k3)

    # Add elements 
    for nd in (n1,n2,n3,n4):
        m1.add_node(nd)
    for el in (e1,e2,e3):
        m1.add_element(el)

    m1.add_force(n4,(P,))
    m1.add_constraint(n1,ux=0)
    m1.add_constraint(n2,ux=0)
    m1.solve()
    
    # a) Global matrix
    print("a) Global matrix:\n {0}".format(m1.KG))
    # b) Nodal displacement -> 3 and 4
    print("\nb) Nodal displacement (3 and 4)")
    print("UX3: {0}".format(n3.ux))
    print("UX4: {0}".format(n4.ux))
    # c) Reaction forces (1 and 2)
    print("\nc) Nodal forces (1 and 2)")
    print("FX1: {0}".format(n1.fx))
    print("FX2: {0}".format(n2.fx))
    # d) Forces in each spring
    print("\nd) Element forces")
    print("FE1:\n {0}".format(e1.fx))
    print("FE2:\n {0}".format(e2.fx))
    print("FE3:\n {0}".format(e3.fx))

if __name__ == '__main__':
    test1()
