# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  License: MIT License
# ***********************************
from nusa.core import *
from nusa.model import *
from nusa.element import *

def test1():
    """
    Logan, D. (2007). A first course in the finite element analysis.
    Example 3.1, pp. 70.
    """
    # Input data 
    E1 = 30e6
    A1 = 1.0
    E2 = 15e6
    A2 = 2.0
    L = 30.0
    P = 3000.0
    # Model
    m1 = BarModel("Bar Model")
    # Nodes
    n1 = Node((0,0))
    n2 = Node((30,0))
    n3 = Node((60,0))
    n4 = Node((90,0))
    # Elements
    e1 = Bar((n1,n2),E1,A1)
    e2 = Bar((n2,n3),E1,A1)
    e3 = Bar((n3,n4),E2,A2)

    # Add elements 
    for nd in (n1,n2,n3,n4):
        m1.add_node(nd)
    for el in (e1,e2,e3):
        m1.add_element(el)

    m1.add_force(n2,(P,))
    m1.add_constraint(n1,ux=0) # fixed 
    m1.add_constraint(n4,ux=0) # fixed
    m1.solve() # Solve model
    
    print("Node | Displacements | Forces")
    for node in m1.get_nodes():
        print("{0}\t{1:0.6f}\t{2}".format(node.label, node.ux, node.fx)) 
    
    
if __name__ == '__main__':
    test1()
