# ***********************************
#  Author: Pedro Jorge De Los Santos    
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************
from nusa.core import Element, Model
import nusa.core as nc
import numpy as np
import numpy.linalg as la

class Node(nc.Node):
    def __init__(self,coordinates):
        nc.Node.__init__(self,coordinates)
        self.z = coordinates[2]


class LT3D4(Element):
    """
    Truss element for finite element analysis
    
    *nodes* : Tuple of :class:`~nusa.core.Node`
        Connectivity for element
    
    *E* : float
        Young modulus
        
    *nu* : float
        Poisson's Ratio
    """
    def __init__(self,nodes,E,nu):
        Element.__init__(self,etype="LT3D4")
        self.nodes = nodes
        self.E = E
        self.nu = nu
        
    @property
    def V(self):
        n1,n2,n3,n4 = self.getNodes()
        V = np.array([[1, n1.x, n1.y, n1.z],
                      [1, n2.x, n2.y, n2.z],
                      [1, n3.x, n3.y, n3.z],
                      [1, n4.x, n4.y, n4.z]])
        return la.det(V)/6
        
    @property
    def D(self):
        E,nu = self.E, self.nu
        mult = E/((1+nu)*(1-2*nu))
        k1 = 1-nu
        k2 = (1-2*nu)/2
        M = np.array([
                     [k1, nu, nu,  0,  0,  0],
                     [nu, k1, nu,  0,  0,  0],
                     [nu, nu, k1,  0,  0,  0],
                     [ 0,  0,  0, k2,  0,  0],
                     [ 0,  0,  0,  0, k2,  0],
                     [ 0,  0,  0,  0,  0, k2]])
        return mult*M
    
    @property
    def B(self):
        n1,n2,n3,n4 = self.getNodes()
        x1,y1,z1 = n1.x, n1.y, n1.z
        x2,y2,z2 = n2.x, n2.y, n2.z
        x3,y3,z3 = n3.x, n3.y, n3.z
        x4,y4,z4 = n4.x, n4.y, n4.z
        a1 = np.array([[x2,y2,z2], [x3,y3,z3], [x4,y4,z4]])
        a2 = np.array([[x2,y2,z2], [x3,y3,z3], [x4,y4,z4]])
        
        
    def get_element_stiffness(self):
        pass
        
    def get_nodes(self):
        return self.nodes
        


if __name__=='__main__':
    E, nu = 200e9, 0.3
    coords = {1:(0,0,0),
              2:(1,0,0),
              3:(0,1,0),
              4:(0,0,1)}
    nodos = []
    for n in coords.keys():
        x,y,z = coords[n][0], coords[n][1], coords[n][2]
        nd = Node((x,y,z))
        nodos.append(nd)
    e1 = LT3D4(nodos,E,nu)
    print(e1.V, e1.D)
    
    
    
    
    
    
