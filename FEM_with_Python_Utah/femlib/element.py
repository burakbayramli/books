from math import sqrt
import numpy as np

from shapefun import LinearGaussLegendre1D

ROOT3 = sqrt(3.)

__all__ = ['LinearElement', 'Element']

class FiniteElement(object):
    order = None
    name = None
    num_points = None
    num_dof_per_node = None
    num_gauss = None
    gauss_weights = None
    gauss_points = None

    def iso_p_map(self, xi, coords=None):
        '''Isoparametric mapping from natural coordinate to spatial coordinates

        Parameters
        ----------
        xi : real
            Point in natural coordinates
        coords : ndarray, optional [None]
            Coordinates of element nodes. If not given, initial coordinates
            are used

        Returns
        -------
        x : real
            xi in spatial coordinates

        '''
        if coords is None:
            coords = self.X
        return np.dot(self.shape.eval(xi), coords)

    def jacobian(self, xi, coords=None):
        """Compute the Jacobian of the element deformation

        Parameters
        ----------
        xi : real
            Gauss point
        coords : ndarray
            Nodal coordinates, [left_node, right_node]

        Returns
        -------
        jac : real
            Jacobian of the deformation

        Notes
        -----
        The Jacobian is defined by

                  dN
            J =  --- . x
                 dxi

        """
        if coords is None:
            coords = self.X
        df = self.shape.grad(xi)
        return np.dot(df, coords)

    def integrate(self, f1=None, f2=None, coords=None,
                  derivative=False, atgauss=(False, False)):
        '''Perform integration over this element

        Parameters
        ----------
        f1, f2 : callables, optional [None]
            Functions to evaluate. If atgauss[1,2] is False (default) f[1,2]
            is assumed to be defined in the global coordinates, otherwise it
            is defined in the natural coordinates
        coords : ndarray
            Current element coordinates
        derivative : bool, optional [False]
            Use shape function or its derivative in integration
        atgauss : list of bool [False, False]
            f1 and or f2 are functions defined at the Gauss points

        Returns
        -------
        L : ndarray (num_points,)
            The value of the integral at each node

        Notes
        -----
        Integrate  either
              integrate(phi(xi) f1(xi) f2(xi), (x, xa, xb))
        or
              integrate(dphi(xi) f1(xi) f2(xi), (x, xa, xb))
        over this element, depending on if derivative is False or True.

        '''
        num_dof = self.num_points * self.num_dof_per_node
        t = np.array(self.gauss_weights)

        if f1 is not None:
            # Accumulate value at Gauss points
            for (n, xi) in enumerate(self.gauss_points):
                if atgauss[0]: t[n] *= f1(xi)
                else: t[n] *= f1(self.iso_p_map(xi))

        if f2 is not None:
            # Accumulate value at Gauss points
            for (n, xi) in enumerate(self.gauss_points):
                if atgauss[1]: t[n] *= f2(xi)
                else: t[n] *= f2(self.iso_p_map(xi))

        if derivative:
            A = lambda xi: self.shape.grad(xi)
        else:
            A = lambda xi: self.shape.eval(xi) * self.jacobian(xi, coords=coords)

        q = np.zeros(num_dof)
        for (n, xi) in enumerate(self.gauss_points):
            q += t[n] * A(xi)

        return q

class LinearElement(FiniteElement):
    order = 1
    name = 'Link2'
    type = 'TRUSS'
    num_points = 2
    num_dof_per_node = 1
    num_gauss = 2
    gauss_weights = np.ones(2)
    gauss_points = np.array([-1., 1.]) / ROOT3
    var_names = ('E', 'S')
    def __init__(self, label, connect, vertices, coeff=None):
        '''Linear Lagrange element'''
        self.label = label
        self.connect = connect
        self.X = vertices
        self.shape = LinearGaussLegendre1D()
        self.num_dof = self.num_dof_per_node * self.num_points
        if coeff is None:
            self.coeff = lambda a: 1.
        else:
            self.coeff = lambda x, a=coeff: a / (x[1] - x[0])
        self.variables = [0, 0]

    def stiffness(self, coords=None):
        if coords is None:
            coords = self.X
        return self.coeff(coords)

    def update(self, u):
        dL = u[1] - u[0]
        L = self.X[1] - self.X[0]
        eps = dL / L
        sig = self.stiffness() * eps
        self.variables = [eps, sig]

def Element(type='Link2', coeff=None):
    '''Factory method that delays instantiation of the element

    Parameters
    ----------
    type : string, optional [Link2]
        The name of the element type
    coeff :

    Returns
    -------
    create_element : callable
        A function that creates the element

    Notes
    -----
    The delayed instantiation is so that a block of elements can be assigned
    an element type (through this function) and later each element in the
    block can create its own element instance.

    '''
    klasses = [LinearElement]
    try:
        from myelement import QuadraticElement
        klasses.append(QuadraticElement)
    except ImportError:
        print 'myelement module not imported'
    for klass in klasses:
        if type.lower() == klass.name.lower():
            break
    else:
        raise ValueError('{0} is not a recognized element type'.format(type))

    def create_element(*args):
        kwds = {'coeff': coeff}
        element = klass(*args, **kwds)
        return element

    return create_element
