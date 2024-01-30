# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  License: MIT License
# ***********************************
from nusa.core import *
from nusa.model import *
from nusa.element import *

def simple_case():
    P = 750
    k = 300
    # Model
    ms = SpringModel("Simple")
    # Nodes
    n1 = Node((0,0))
    n2 = Node((0,0))
    # Elements
    e1 = Spring((n1,n2),k)
    
    for nd in (n1,n2):
        ms.add_node(nd)
    ms.add_element(e1)
    
    ms.add_force(n2,(P,))
    ms.add_constraint(n1,ux=0)
    ms.solve()
    
    # print("Node displacements")
    # for n in ms.get_nodes():
    #     print(n.ux, n.uy)

    print(ms.simple_report())

if __name__ == '__main__':
    simple_case()

