# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************
import numpy as np
from .core import Element
import nusa.templates as tmp
from scipy.sparse import csr_matrix

class Spring(Element):
    """
    Spring element for finite element analysis
    
    *nodes* : tuple
        Connectivity for element given as tuple of 
        :class:`~core.base.Node` objects
    
    *ke* : float
        Spring stiffness
    
    Example ::
    
        n1 = Node((0,0))
        n2 = Node((0,0))
        e1 = Spring((n1,n2), 1000)
    
    """
    def __init__(self,nodes,ke):
        Element.__init__(self, etype="spring")
        self.nodes = nodes
        self.k = ke
    
    @property
    def fx(self):
        ke = self.get_element_stiffness() # Element stiffness
        n1, n2 = self.get_nodes()
        un = np.array([[n1.ux],[n2.ux]]) # Nodal displacements
        return np.dot(ke, un) # Return  {fxe} = [Ke]{uxe}
        
    @fx.setter
    def fx(self,val):
        self._fx = val
        
    def get_element_stiffness(self):
        r"""
        Get stiffness matrix for this element.
        
        The stiffness matrix for a spring element is defined by:
        
        .. math::
        
            [k]_e = \begin{bmatrix}
            k  & -k \\
            -k &  k \\
            \end{bmatrix}
        
        where *k* is the spring stiffness.
        
        Return a numpy array.
        """
        self._KE = np.array([[self.k,-self.k],[-self.k,self.k]])
        return self._KE
    
    def get_global_stiffness(self,msz):
        pass
        #~ ni, nj = self.get_nodes()
        #~ self.keg = np.zeros((msz,msz))
        #~ idx = np.ix_([ni.label, nj.label],[ni.label, nj.label])
        #~ row = np.array([ni.label, ni.label, nj.label, nj.label])
        #~ col = np.array([ni.label, nj.label, ni.label, nj.label])
        #~ data = self.get_element_stiffness().reshape(-1)
        #~ print data, row, col
        #~ self.keg =  csr_matrix((data, (row, col)), shape=(msz,msz)).toarray()
        #~ return self.keg
    
    def get_nodes(self):
        """
        Returns a tuple of nodes
        """
        return self.nodes



class Bar(Element):
    """
    Bar element for finite element analysis
    
    *nodes* : :class:`~core.base.Node`
        Connectivity for element
    
    *E* : float
        Young's modulus
        
    *A* : float
        Area of element
    """
    def __init__(self,nodes,E,A):
        Element.__init__(self,etype="bar")
        self.nodes = nodes
        self.E = E # Elastic modulus
        self.A = A # Cross-section
        
    @property
    def fx(self):
        """
        Compute force in x-dir (axial-dir)
        """
        ke = self.get_element_stiffness() # Element stiffness
        n1, n2 = self.get_nodes()
        un = np.array([[n1.ux],[n2.ux]]) # Nodal displacements
        return np.dot(ke, un) # Return  {fxe} = [Ke]{uxe}
        
    @fx.setter
    def fx(self,val):
        self._fx = val
        
    @property
    def sx(self):
        r"""
        Compute normal stress in x-dir
        
        Given by:
        
        .. math::
            
            S_x = K \left( \frac{u}{A} \right)
        
        where
        
        * K - Element stiffness matrix
        * u - Nodal displacements
        * A - Cross-section of element
        """
        ke = self.get_element_stiffness() # Element stiffness
        na, nb = self.get_nodes()
        u = np.array([na.ux, nb.ux]) # Nodes displacements
        sx = np.dot(ke, u/self.A) # matrix multiplication
        return sx
        
    @sx.setter
    def sx(self,val):
        self._sx = val
    
    @property
    def L(self):
        """
        Length of element
        """
        ni,nj = self.get_nodes()
        x0,x1,y0,y1 = ni.x, nj.x, ni.y, nj.y
        _l = np.sqrt( (x1-x0)**2 + (y1-y0)**2 )
        return _l

    def get_element_stiffness(self):
        r"""
        Get stiffness matrix for this element
        
        The stiffness matrix for bar element is given by:
        
        .. math::
        
            [k]_e = \frac{AE}{L} \begin{bmatrix} 1 & -1 \\ -1 & 1 \end{bmatrix}
        
        where
        
        * A - Cross-section of element
        * E - Young's Modulus
        * L - Length of element
        """
        self._KE = (self.A*self.E/self.L)*np.array([[1,-1],[-1,1]])
        return self._KE
        
    def get_nodes(self):
        """
        Returns a tuple of nodes
        """
        return self.nodes





class Truss(Element):
    """
    Truss element for finite element analysis
    
    *nodes* : Tuple of :class:`~nusa.core.Node`
        Connectivity for element
    
    *E* : float
        Young modulus
        
    *A* : float
        Area of element
    """
    def __init__(self,nodes,E,A):
        Element.__init__(self,etype="truss")
        self.nodes = nodes
        self.E = E
        self.A = A
        
    @property
    def L(self):
        """
        Length of element
        """
        ni,nj = self.get_nodes()
        x0,x1,y0,y1 = ni.x, nj.x, ni.y, nj.y
        _l = np.sqrt( (x1-x0)**2 + (y1-y0)**2 )
        return _l
    
    @property
    def theta(self):
        """
        Element angle, measure from X-positive axis counter-clockwise.
        """
        ni,nj = self.get_nodes()
        x0,x1,y0,y1 = ni.x, nj.x, ni.y, nj.y
        # ~ if x0==x1:
            # ~ theta = 90*(np.pi/180)
        # ~ else:
        theta = np.arctan2((y1-y0),(x1-x0))
        return theta
    
    @property
    def f(self):
        r"""
        Force in this element, given by
        
        .. math::
        
            f = \frac{EA}{L}\begin{bmatrix} C & S & -C & -S \end{bmatrix}\left\{u\right\}
        
        where:
        
            * E - Elastic modulus
            * A - Cross-section
            * L - Length
            * C - :math:`\cos(\theta)`
            * S - :math:`\sin(\theta)`
            * u - Four-element vector of nodal displacements -> :math:`\left\{ ux_i; uy_i; ux_j; uy_j \right\}`
        """
        return self._compute_force()
    
    @property
    def s(self):
        """
        Stress in this element, given by:
        
        s = f/A
        """
        s = self.f/self.A
        return s
        
    def _compute_force(self):
        theta = self.theta
        E, A, L = self.E, self.A, self.L
        C = np.cos(theta)
        S = np.sin(theta)
        ni, nj = self.get_nodes()
        u = np.array([ni.ux, ni.uy, nj.ux, nj.uy]).T
        F = (E*A/L)*np.dot(np.array([-C, -S, C, S]), u)
        return F
        
    def get_element_stiffness(self):
        """
        Get stiffness matrix for this element
        """
        multiplier = (self.A*self.E/self.L)
        C = np.cos(self.theta)
        S = np.sin(self.theta)
        CS = C*S
        self._K = multiplier*np.array([[C**2 , CS   , -C**2, -CS  ],
                                       [CS   , S**2 , -CS  , -S**2],
                                       [-C**2, -CS  , C**2 , CS   ],
                                       [-CS  , -S**2,  CS  , S**2 ]])
        return self._K
        
    def get_nodes(self):
        return self.nodes



class Beam(Element):
    """
    Beam element for finite element analysis
    
    *nodes* : :class:`~core.base.Node`
        Connectivity for element
    
    *E* : float
        Young's modulus
        
    *I* : float
        Moment of inertia
    
    """
    def __init__(self,nodes,E,I):
        Element.__init__(self,etype="beam")
        self.nodes = nodes
        self.E = E
        self.I = I
        
    def get_element_stiffness(self):
        """
        Get stiffness matrix for this element
        
        """
        multiplier = (self.I*self.E/self.L**3)
        a = 6*self.L
        b = 4*self.L**2
        c = 2*self.L**2
        self._K = multiplier*np.array([[ 12, a, -12, a],
                                       [  a, b,  -a, c],
                                       [-12,-a,  12,-a],
                                       [  a, c,  -a, b]])
        return self._K

    def _compute_element_forces(self):
        """
        Just that 
        
        Set fy and m properties.
        """
        ke = self.get_element_stiffness() # Element stiffness
        n1, n2 = self.get_nodes()
        un = np.array([[n1.uy, n1.ur, n2.uy, n2.ur]]).transpose() # Nodal displacements
        EF = np.dot(ke, un) # Return  {fxe} = [Ke]{uxe}
        self.fy = EF[::2] # Set fy
        self.m = EF[1::2] # Set m
        
    @property
    def fy(self):
        """
        Compute y-force 
        """
        self._compute_element_forces()
        return self._fy
        
    @fy.setter
    def fy(self,val):
        self._fy = val
        
    @property
    def m(self):
        """
        Compute moment 
        """
        self._compute_element_forces()
        return self._m
        
    @m.setter
    def m(self,val):
        self._m = val

    @property
    def L(self):
        """
        Length of element
        """
        ni,nj = self.get_nodes()
        x0,x1,y0,y1 = ni.x, nj.x, ni.y, nj.y
        _l = np.sqrt( (x1-x0)**2 + (y1-y0)**2 )
        return _l
        
    def get_nodes(self):
        return self.nodes



class LinearTriangle(Element):
    """
    Linear triangle element for finite element analysis
    
    *nodes* : :class:`~nusa.core.Node`
        Connectivity for element
    
    *E* : float
        Young's modulus
        
    *nu* : float
        Poisson ratio
        
    *t* : float
        Thickness
    
    Example::
        n1 = Node((0,0))
        n2 = Node((0.5,0))
        n3 = Node((0.5,0.25))
        e1 = LinearTriangle((n1,n2,n3),210e9, 0.3, 0.025)
    """
    def __init__(self,nodes,E,nu,t):
        Element.__init__(self,etype="triangle")
        self.nodes = nodes
        self.E = E
        self.nu = nu
        self.t = t
        self._sx = 0
        self._sy = 0
        self._sxy = 0
        
    @property
    def sx(self):
        _sx,_sy,_sxy = self.get_element_stresses()
        self._sx = _sx
        return self._sx
    
    @sx.setter
    def sx(self,val):
        self._sx = val
        
    @property
    def sy(self):
        _sx,_sy,_sxy = self.get_element_stresses()
        self._sy = _sy
        return self._sy
    
    @sy.setter
    def sy(self,val):
        self._sy = val
    
    @property
    def sxy(self):
        _sx,_sy,_sxy = self.get_element_stresses()
        self._sxy = _sxy
        return self._sxy
    
    @sxy.setter
    def sxy(self,val):
        self._sxy = val
    
    @property
    def ex(self):
        ex,ey,exy = self.get_element_strains()
        self._ex = ex
        return self._ex
    
    @ex.setter
    def ex(self,val):
        self._ex = val
    
    @property
    def ey(self):
        ex,ey,exy = self.get_element_strains()
        self._ey = ey
        return self._ey
    
    @ey.setter
    def ey(self,val):
        self._ey = val
    
    @property
    def exy(self):
        ex,ey,exy = self.get_element_strains()
        self._exy = exy
        return self._exy
    
    @exy.setter
    def exy(self,val):
        self._exy = val
    
    @property
    def D(self):
        """
        Constitutive matrix 
        
        Currently only plane stress supported
        """
        nu, E = self.nu, self.E
        D = (E/(1-nu**2))*np.array([[1, nu, 0],
                            [nu, 1, 0],
                            [0, 0, (1-nu)/2]
                            ])
        return D
    
    @property
    def B(self):
        ni, nj, nm = self.nodes
        A = self.A
        betai = nj.y - nm.y
        betaj = nm.y - ni.y
        betam = ni.y - nj.y
        gammai = nm.x - nj.x
        gammaj = ni.x - nm.x
        gammam = nj.x - ni.x
        B = (1/(2*A))*np.array([[betai, 0, betaj, 0, betam, 0],
                                 [0, gammai, 0, gammaj, 0, gammam],
                                 [gammai, betai, gammaj, betaj, gammam, betam]
                                 ])
        return B
    
    @property
    def A(self):
        n1, n2, n3 = self.nodes
        xi, yi = n1.x, n1.y
        xj, yj = n2.x, n2.y
        xm, ym = n3.x, n3.y
        A = (xi*(yj-ym) + xj*(ym-yi) + xm*(yi-yj))/2
        return A
    
    def get_element_stiffness(self):
        """
        Get stiffness matrix for this element
        """
        ni, nj, nm = self.nodes
        A, nu, t, E = self.A, self.nu, self.t, self.E
        B, D = self.B, self.D
        return t*A*np.dot(np.dot(B.T,D),B)
        
    def get_element_stresses(self):
        ni, nj, nm = self.nodes
        A, nu, t, E = self.A, self.nu, self.t, self.E
        u = np.array([ni.ux,ni.uy,nj.ux,nj.uy,nm.ux,nm.uy])
        B, D = self.B, self.D
        return np.dot(np.dot(D,B),u)
        
    def get_element_strains(self):
        ni, nj, nm = self.nodes
        A, nu, t, E = self.A, self.nu, self.t, self.E 
        u = np.array([ni.ux,ni.uy,nj.ux,nj.uy,nm.ux,nm.uy])
        B = self.B
        return np.dot(B,u)
        
    def get_nodes(self):
        return self.nodes




if __name__=='__main__':
    pass
