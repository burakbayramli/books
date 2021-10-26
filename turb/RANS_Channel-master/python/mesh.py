


#**************************************************************************
#       mesh function, mesh generator and its derivatives
#**************************************************************************
#
# The mesh and its derivatives are generated with the....
#
#  Bottom wall                   Channel center                  Top wall
#     y=0                           y=H/2                         y=H
#       || |  |   |    |      |      |      |      |    |   |  | ||
#
#
# Inputs:
#   H        channel height
#   n        number of mesh points (nodes)
#   fact     factor to set mesh clustering at the wall
#   discr    discretization
#
# Output:
#   MESH     mesh structure containing: 
#            'y'      ... y coordinates
#            'ddy'    ... first derivative coefficient matrix d()/dy
#            'd2dy2'  ... second derivative coefficient matrix d2()/dy2
#


    
import numpy as np
from scipy import sparse
import math as m



def finiteDiffCoeff(x,k):

    n = np.size(x)
    A = np.ones((n,n))

    for i in range(1, n):
        A[i,:] = pow(x,i)/m.factorial(i)

    b = np.zeros((n,1))            # b is right hand side,
    b[k] = 1                       # so k'th derivative term remains
    sol = np.linalg.solve(A, b)   # solve system for coefficients
    return sol.transpose()
    
    


class Mesh:
    def __init__(self, n, H, fact, ns):

        self.nPoints = n
        
        di = 1.0/(n-1)
        i = (np.linspace(0,n-1,n))/(n-1) - 0.5
        
        # y - coordinate: tanh clustering at the walls
        self.y = H * (1.0 + np.tanh(fact*i)/m.tanh(fact/2))/2.0
        
        # coordinate transformation: derivative of y with respect to 'i'
        dydi =  H * fact/2.0/np.tanh(fact/2)/np.power(np.cosh(fact*i), 2.0)
        
        # coordinate transformation: second derivative of y with respect to 'i'
        d2ydi2 = -H * np.power(fact,2.0)/np.tanh(fact/2)*np.tanh(fact*i)/np.power(np.cosh(fact*i),2.0)
                
        # -------------------------------------------------------------
        # coefficient matrix for d()/dy
        # du/dy = 1/(dy/di) * du/di
        ddy = np.zeros((n,n))
        
        ddy[0,  0:7]   = finiteDiffCoeff(np.arange( 0,7), 1)
        ddy[1,  0:7]   = finiteDiffCoeff(np.arange(-1,6), 1)
        ddy[2,  0:7]   = finiteDiffCoeff(np.arange(-2,5), 1)
        ddy[n-3,n-7:n] = finiteDiffCoeff(np.arange(-4,3), 1)
        ddy[n-2,n-7:n] = finiteDiffCoeff(np.arange(-5,2), 1)
        ddy[n-1,n-7:n] = finiteDiffCoeff(np.arange(-6,1), 1)
        
        for i in range(ns,n-ns):
            ddy[i,:] = 0.0
            ddy[i,i-ns:i+ns+1] = finiteDiffCoeff( np.arange(-ns,ns+1), 1)
            
        # multiply coordinate transformation 
        for i in range(0,n):
            ddy[i,:] = ddy[i,:] * 1/di/dydi[i];
            
#        self.ddy = sparse.csr_matrix(ddy)
        self.ddy = ddy

        
        # -------------------------------------------------------------
        # coefficient matrix for d2()/dy2 (second derivative)
        # d2u/dy2 = 1/(dy/di)^2*d2u/di2 - 1/(dy/di)^3*d2y/di2*du/di
        d2dy2 = np.zeros((n,n))
        
        d2dy2[0,  0:7  ] = finiteDiffCoeff(np.arange( 0,7), 2)
        d2dy2[1,  0:7  ] = finiteDiffCoeff(np.arange(-1,6), 2)
        d2dy2[2,  0:7  ] = finiteDiffCoeff(np.arange(-2,5), 2)
        d2dy2[n-3,n-7:n] = finiteDiffCoeff(np.arange(-4,3), 2)
        d2dy2[n-2,n-7:n] = finiteDiffCoeff(np.arange(-5,2), 2)
        d2dy2[n-1,n-7:n] = finiteDiffCoeff(np.arange(-6,1), 2)
        
        for i in range(ns,n-ns):
            d2dy2[i,:] = 0.0
            d2dy2[i,i-ns:i+ns+1] = finiteDiffCoeff( np.arange(-ns,ns+1), 2)
        
        # multiply coordinate transformation 
        for i in range(0,n):
            d2dy2[i,:] = d2dy2[i,:]/np.power(di*dydi[i], 2.0) - ddy[i,:]*d2ydi2[i]/np.power(dydi[i],2)

#        self.d2dy2 = sparse.csr_matrix(d2dy2)
        self.d2dy2 = d2dy2





