# Pydgeon - the Python DG Environment
# (C) 2009, 2010 Tim Warburton, Xueyu Zhu, Andreas Kloeckner
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.




from __future__ import division

from math import sqrt
import numpy as np
import numpy.linalg as la

from pydgeon.tools import fact, NODETOL




# {{{ helper routines

def JacobiP(x, alpha, beta, N):
    """ function P = JacobiP(x, alpha, beta, N)
         Purpose: Evaluate Jacobi Polynomial of type (alpha, beta) > -1
                  (alpha+beta <> -1) at points x for order N and
                  returns P[1:length(xp))]
         Note   : They are normalized to be orthonormal."""
    N = np.int32(N)
    Nx = len(x)
    if x.shape[0]>1:
        x = x.T
    # Storage for recursive construction
    PL = np.zeros((np.int32(Nx), np.int32(N+1)))

    # Initial values P_0(x) and P_1(x)
    gamma0 = np.power(2., alpha+beta+1)/(alpha+beta+1.)*fact(alpha+1)*fact(beta+1)/fact(alpha+beta+1)

    #
    PL[:,0] = 1.0/sqrt(gamma0)
    if N==0:
        return PL[:,0]

    gamma1 = (alpha+1)*(beta+1)/(alpha+beta+3)*gamma0
    PL[:,1] = ((alpha+beta+2)*x/2 + (alpha-beta)/2)/sqrt(gamma1)
    if N==1:
        return PL[:,1]

    # Repeat value in recurrence.
    aold = 2./(2.+alpha+beta)*sqrt((alpha+1.)*(beta+1.)/(alpha+beta+3.))

    # Forward recurrence using the symmetry of the recurrence.
    for i in range(1, N):
            h1 = 2.*i+alpha+beta

            foo = (i+1.)*(i+1.+alpha+beta)*(i+1.+alpha)*(i+1.+beta)/(h1+1.)/(h1+3.)
            anew = 2./(h1+2.)*sqrt(foo)

            bnew = -(alpha*alpha-beta*beta)/(h1*(h1+2.))
            PL[:, i+1] = ( -aold*PL[:, i-1] + np.multiply(x-bnew, PL[:, i]) )/anew
            aold = anew

    return PL[:, N]

def Vandermonde1D(N, xp):
    """Initialize the 1D Vandermonde Matrix.
    V_{ij} = phi_j(xp_i)
    """

    Nx = np.int32(xp.shape[0])
    N  = np.int32(N)
    V1D = np.zeros((Nx, N+1))

    for j in range(N+1):
            V1D[:, j] = JacobiP(xp, 0, 0, j).T # give the tranpose of Jacobi.p

    return V1D

def JacobiGQ(alpha, beta, N):
    """Compute the N'th order Gauss quadrature points, x,
    and weights, w, associated with the Jacobi
    polynomial, of type (alpha, beta) > -1 ( <> -0.5).
    """

    if N==0:
        return np.array([(alpha-beta)/(alpha+beta+2)]), np.array([2])

    # Form symmetric matrix from recurrence.
    J    = np.zeros(N+1)
    h1   = 2*np.arange(N+1) + alpha + beta
    temp = np.arange(N) + 1.0
    J    = np.diag(-1.0/2.0*(alpha**2-beta**2)/(h1+2.0)/h1) + np.diag(2.0/(h1[0:N]+2.0)*np.sqrt(temp*(temp+alpha+beta)*(temp+alpha)*(temp+beta)/(h1[0:N]+1.0)/(h1[0:N]+3.0)),1)

    if alpha+beta < 10*np.finfo(float).eps :
        J[0,0] = 0.0
    J = J + J.T

    # Compute quadrature by eigenvalue solve
    D, V = la.eig(J)
    ind = np.argsort(D)
    D = D[ind]
    V = V[:, ind]
    x = D
    w = (V[0,:].T)**2*2**(alpha+beta+1)/(alpha+beta+1)*fact(alpha+1)*fact(beta+1)/fact(alpha+beta+1)

    return x, w

def JacobiGL(alpha, beta, N):
    """Compute the Nth order Gauss Lobatto quadrature points, x,
    associated with the Jacobi polynomial, of type (alpha, beta) > -1 ( <> -0.5).
    """

    x = np.zeros((N+1,1))
    if N==1:
        x[0]=-1.0
        x[1]=1.0
        return x

    xint, w = JacobiGQ(alpha+1, beta+1, N-2)

    x = np.hstack((-1.0, xint,1.0))

    return x.T



def Warpfactor(N, rout):
    """Compute scaled warp function at order N based on
    rout interpolation nodes.
    """

    # Compute LGL and equidistant node distribution
    LGLr = JacobiGL(0,0, N); req  = np.linspace(-1,1, N+1)
    # Compute V based on req
    Veq = Vandermonde1D(N, req)
    # Evaluate Lagrange polynomial at rout
    Nr = len(rout); Pmat = np.zeros((N+1, Nr))
    for i in range(N+1):
        Pmat[i,:] = JacobiP(rout.T[0,:], 0, 0, i)

    Lmat = la.solve(Veq.T, Pmat)

    # Compute warp factor
    warp = np.dot(Lmat.T, LGLr - req)
    warp = warp.reshape(Lmat.shape[1],1)
    zerof = (abs(rout)<1.0-1.0e-10)
    sf = 1.0 - (zerof*rout)**2
    warp = warp/sf + warp*(zerof-1)
    return warp

def Nodes2D(N):
    """Compute (x, y) nodes in equilateral triangle for polynomial
    of order N.
    """

    alpopt = np.array([0.0000, 0.0000, 1.4152, 0.1001, 0.2751, 0.9800, 1.0999,\
            1.2832, 1.3648, 1.4773, 1.4959, 1.5743, 1.5770, 1.6223,1.6258])

    # Set optimized parameter, alpha, depending on order N
    if N< 16:
        alpha = alpopt[N-1]
    else:
        alpha = 5.0/3.0

    # total number of nodes
    Np = (N+1)*(N+2)//2

    # Create equidistributed nodes on equilateral triangle
    L1 = np.zeros((Np,1)); L2 = np.zeros((Np,1)); L3 = np.zeros((Np,1))
    sk = 0
    for n in range(N+1):
        for m in range(N+1-n):
            L1[sk] = n/N
            L3[sk] = m/N
            sk = sk+1

    L2 = 1.0-L1-L3
    x = -L2+L3; y = (-L2-L3+2*L1)/sqrt(3.0)

    # Compute blending function at each node for each edge
    blend1 = 4*L2*L3; blend2 = 4*L1*L3; blend3 = 4*L1*L2

    # Amount of warp for each node, for each edge
    warpf1 = Warpfactor(N, L3-L2)
    warpf2 = Warpfactor(N, L1-L3)
    warpf3 = Warpfactor(N, L2-L1)

    # Combine blend & warp
    warp1 = blend1*warpf1*(1 + (alpha*L1)**2)
    warp2 = blend2*warpf2*(1 + (alpha*L2)**2)
    warp3 = blend3*warpf3*(1 + (alpha*L3)**2)

    # Accumulate deformations associated with each edge
    x = x + 1*warp1 + np.cos(2*np.pi/3)*warp2 + np.cos(4*np.pi/3)*warp3
    y = y + 0*warp1 + np.sin(2*np.pi/3)*warp2 + np.sin(4*np.pi/3)*warp3
    return x, y

def xytors(x, y):
    """From (x, y) in equilateral triangle to (r, s) coordinates in standard triangle."""

    L1 = (np.sqrt(3.0)*y+1.0)/3.0
    L2 = (-3.0*x - np.sqrt(3.0)*y + 2.0)/6.0
    L3 = ( 3.0*x - np.sqrt(3.0)*y + 2.0)/6.0

    r = -L2 + L3 - L1; s = -L2 - L3 + L1
    return r, s

def rstoab(r, s):
    """Transfer from (r, s) -> (a, b) coordinates in triangle.
    """

    Np = len(r)
    a = np.zeros((Np,1))
    for n in range(Np):
        if s[n] != 1:
            a[n] = 2*(1+r[n])/(1-s[n])-1
        else:
            a[n] = -1

    b = s
    return a, b

def Simplex2DP(a, b, i, j):
    """Evaluate 2D orthonormal polynomial
    on simplex at (a, b) of order (i, j).
    """

    h1 = JacobiP(a,0,0, i).reshape(len(a),1)
    h2 = JacobiP(b,2*i+1,0, j).reshape(len(a),1)
    P  = np.sqrt(2.0)*h1*h2*(1-b)**i
    return P[:,0]

def Vandermonde2D(N, r, s):
    """Initialize the 2D Vandermonde Matrix,  V_{ij} = phi_j(r_i, s_i)
    """

    V2D = np.zeros((len(r),(N+1)*(N+2)/2))

    # Transfer to (a, b) coordinates
    a, b = rstoab(r, s)

    # build the Vandermonde matrix
    sk = 0

    for i in range(N+1):
        for j in range(N-i+1):
            V2D[:, sk] = Simplex2DP(a, b, i, j)
            sk = sk+1
    return V2D

def GradJacobiP(z, alpha, beta, N):
    """Evaluate the derivative of the orthonormal Jacobi polynomial
    of type (alpha, beta)>-1, at points x for order N and
    returns dP[1:len(xp))].
    """
    Nx = np.int32(z.shape[0])
    dP = np.zeros((Nx, 1))
    if N==0:
        dP[:,0] = 0.0
    else:
        dP[:,0]= sqrt(N*(N+alpha+beta+1.))*JacobiP(z, alpha+1, beta+1, N-1)

    return dP

def GradSimplex2DP(a, b, id, jd):
    """Return the derivatives of the modal basis (id, jd) on the
    2D simplex at (a, b).
    """

    fa  = JacobiP(a, 0, 0, id).reshape(len(a),1)
    dfa = GradJacobiP(a, 0, 0, id)
    gb  = JacobiP(b, 2*id+1,0, jd).reshape(len(b),1)
    dgb = GradJacobiP(b, 2*id+1,0, jd)

    # r-derivative
    # d/dr = da/dr d/da + db/dr d/db = (2/(1-s)) d/da = (2/(1-b)) d/da
    dmodedr = dfa*gb
    if(id>0):
        dmodedr = dmodedr*((0.5*(1-b))**(id-1))

    # s-derivative
    # d/ds = ((1+a)/2)/((1-b)/2) d/da + d/db
    dmodeds = dfa*(gb*(0.5*(1+a)))
    if(id>0):
        dmodeds = dmodeds*((0.5*(1-b))**(id-1))
    tmp = dgb*((0.5*(1-b))**id)
    if(id>0):
        tmp = tmp-0.5*id*gb*((0.5*(1-b))**(id-1))
    dmodeds = dmodeds+fa*tmp
    # Normalize
    dmodedr = 2**(id+0.5)*dmodedr
    dmodeds = 2**(id+0.5)*dmodeds

    return dmodedr[:,0], dmodeds[:,0]


def GradVandermonde2D(N, Np, r, s):
    """Initialize the gradient of the modal basis
    (i, j) at (r, s) at order N.
    """

    V2Dr = np.zeros((len(r), Np))
    V2Ds = np.zeros((len(r), Np))

    # find tensor-product coordinates
    a, b = rstoab(r, s)
    # Initialize matrices
    sk = 0
    for i in range(N+1):
        for j in range(N-i+1):
            V2Dr[:, sk], V2Ds[:, sk] = GradSimplex2DP(a, b, i, j)
            sk = sk+1
    return V2Dr, V2Ds

def Dmatrices2D(N, Np, r, s, V):
    """Initialize the (r, s) differentiation matriceon the simplex,
    evaluated at (r, s) at order N.
    """

    Vr, Vs = GradVandermonde2D(N, Np, r, s)
    invV   = la.inv(V)
    Dr     = np.dot(Vr, invV)
    Ds     = np.dot(Vs, invV)
    return Dr, Ds

def Lift2D(ldis, r, s, V, Fmask):
    """Compute surface to volume lift term for DG formulation."""
    l = ldis

    Emat = np.zeros((l.Np, l.Nfaces*l.Nfp))

    # face 1
    faceR = r[Fmask[0,:]]
    V1D = Vandermonde1D(l.N, faceR)
    massEdge1 = la.inv(np.dot(V1D, V1D.T))
    Emat[Fmask[0,:],0:l.Nfp] = massEdge1

    # face 2
    faceR = r[Fmask[1,:]]
    V1D = Vandermonde1D(l.N, faceR)
    massEdge2 = la.inv(np.dot(V1D, V1D.T))
    Emat[Fmask[1,:], l.Nfp:2*l.Nfp] = massEdge2

    # face 3
    faceS = s[Fmask[2,:]]
    V1D = Vandermonde1D(l.N, faceS)
    massEdge3 = la.inv(np.dot(V1D, V1D.T))
    Emat[Fmask[2,:],2*l.Nfp:3*l.Nfp] = massEdge3

    # inv(mass matrix)*\I_n (L_i, L_j)_{edge_n}
    LIFT = np.dot(V, np.dot(V.T, Emat))
    return LIFT

# }}}




class LocalDiscretization2D:
    def __init__(self, N):
        self.dimensions = 2

        self.Np = (N+1)*(N+2)//2
        self.N = N
        self.Nfp = N+1
        self.Nfaces = 3
        self.Nafp = self.Nfp * self.Nfaces

        # compute nodal set
        x, y = self.x, self.y = Nodes2D(N)
        r, s = self.r, self.s = xytors(self.x, self.y)

        # face masks
        fmask1   = (np.abs(s+1) < NODETOL).nonzero()[0];
        fmask2   = (np.abs(r+s) < NODETOL).nonzero()[0]
        fmask3   = (np.abs(r+1) < NODETOL).nonzero()[0]
        Fmask = self.Fmask = np.vstack((fmask1, fmask2, fmask3))
        FmaskF = self.FmaskF = Fmask.flatten()

        self.Fx = x[FmaskF[:], :]
        self.Fy = y[FmaskF[:], :]

        # Build reference element matrices
        V = self.V  = Vandermonde2D(N, r, s)
        invV = la.inv(self.V)
        MassMatrix = invV.T*invV
        self.Dr, self.Ds = Dmatrices2D(N, self.Np, r, s, self.V)

        self.LIFT = Lift2D(self, r, s, V, Fmask)

        # weak operators
        Vr, Vs = GradVandermonde2D(N, self.Np, r, s)
        invVV = la.inv(np.dot(V, V.T))
        self.Drw = np.dot(np.dot(V, Vr.T), invVV);
        self.Dsw = np.dot(np.dot(V, Vs.T), invVV)

    def gen_submesh_indices(self):
        """Return a list of tuples of indices into the node list that
        generate a tesselation of the reference element.
        """

        node_tuples = [
                (i,j)
                for i in range(self.N+1)
                for j in range(self.N+1-i)
                ]

        node_dict = dict(
                (ituple, idx)
                for idx, ituple in enumerate(node_tuples))

        for i, j in node_tuples:
            if i + j < self.N:
                yield (node_dict[i, j], node_dict[i + 1, j],
                            node_dict[i, j+1])
            if i + j < self.N-1:
                yield (node_dict[i + 1, j+1], node_dict[i, j + 1],
                        node_dict[i + 1, j])

# vim: foldmethod=marker





