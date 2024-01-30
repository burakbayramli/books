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

def test5():
    """
    Beer & Johnston. Mechanics of materials
    Problem 9.75 , pp. 589.
    """
    # Input data 
    E = 29e6 # psi
    b, h = 2.0, 4.0  #in
    I = (1/12.0)*b*h**3
    w = 1e3/12.0 # lb/in
    L1 = 2*12 # in
    L2 = 3*12 #in
    P1 = -1e3 # lb
    P2 = -w*L2/2.0
    P3 = -w*L2/2.0
    M2 = -w*L2**2/12.0
    M3 = w*L2**2/12.0
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
        
    m1.add_force(n1, (P1,))
    m1.add_force(n2, (P2,))
    m1.add_force(n3, (P3,))
    m1.add_moment(n2, (M2,))
    m1.add_moment(n3, (M3,))
    m1.add_constraint(n3, ux=0, uy=0, ur=0) # fixed
    m1.solve() # Solve model
    
    # Slope and deflection in n1
    print("Displacement in node 1: {0}\nSlope in node 1: {1}".format(n1.uy, n1.ur))


if __name__ == '__main__':
    test5()
