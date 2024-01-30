# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  License: MIT License
# ***********************************

from nusa.core import *
from nusa.model import *
from nusa.element import *

def test2():
    """
    Logan, D. (2007). A first course in the finite element analysis.
    Example 2.2, pp. 45.
    """
    P = 4e3
    k = 200e3
    # Model
    m2 = SpringModel("Spring Model 02")
    # Nodes
    n1 = Node((0,0))
    n2 = Node((0,0))
    n3 = Node((0,0))
    n4 = Node((0,0))
    n5 = Node((0,0))
    # Elements
    e1 = Spring((n1,n2),k)
    e2 = Spring((n2,n3),k)
    e3 = Spring((n3,n4),k)
    e4 = Spring((n4,n5),k)
    
    for nd in (n1,n2,n3,n4,n5):
        m2.add_node(nd)
    for el in (e1,e2,e3,e4):
        m2.add_element(el)
    
    m2.add_force(n4,(P,))
    m2.add_constraint(n1,ux=0)
    m2.add_constraint(n5,ux=0.02)
    m2.solve()

    print(f"Global stiffness matrix:\n\n{m2.KG}")
    print(f"Displacements of nodes 2,3 and 4: \n\t{n2.ux}\n\t{n3.ux}\n\t{n4.ux}")
    print(f"Nodal forces: \n\t{n1.fx}\n\t{n2.fx}\n\t{n3.fx}\n\t{n4.fx}\n\t{n5.fx} ")
    print(f"Element forces: \n{e1.fx}\n{e2.fx}\n{e3.fx}\n{e4.fx}")
    

if __name__ == '__main__':
    test2()
