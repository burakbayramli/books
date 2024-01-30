# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  License: MIT License
# ***********************************

from nusa.core import *
from nusa.model import *
from nusa.element import *

def test3():
    """
    Logan, D. (2007). A first course in the finite element analysis.
    Problem 2.8, pp. 62.
    """
    P = 500
    k = 500
    # Model
    m3 = SpringModel("Spring Model 03")
    # Nodes
    n1 = Node((0,0))
    n2 = Node((0,0))
    n3 = Node((0,0))
    # Elements
    e1 = Spring((n1,n2),k)
    e2 = Spring((n2,n3),k)
    
    for nd in (n1,n2,n3):
        m3.add_node(nd)
    for el in (e1,e2):
        m3.add_element(el)
    
    m3.add_force(n3,(P,))
    m3.add_constraint(n1,ux=0)
    m3.solve()
    
    for n in m3.get_nodes():
        print(n.ux, n.uy)
        

if __name__ == '__main__':
    test3()
