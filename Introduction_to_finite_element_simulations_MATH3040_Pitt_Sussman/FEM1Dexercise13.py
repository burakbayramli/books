from FEM1D import *

"""
Use FEM1D to solve PDE u''+2*u'+u=f, x in [0,1]
1. u=1, f=1, Dirichlet b.c. (verification)
2. u=x, f=1+x, Dirichlet b.c. (verification)
3. u=x**2, f=x**2+2*x+2, Dirichlet b.c. (verification)
4. u=(1+x)*exp(1-x)+x*(1-exp(-x)), f=x+2, Neumann conditions

M. M. Sussman 4/16/14
"""

def verification(title,N,rhsfn,exactfn):
    """
    generic verification runs for PDE u''+2*u'+u=rhs on [0,1]
    Dirichlet b.c.
    title= descriptive title
    N=number of elements
    rhsfn=function for rhs as function of x
    exactfn=function for exact solution as function of x
    MMS 4/16/14
    """
    # N elements in [0,1]
    mesh = Mesh(N,0.0,1.0)
    
    # shape functions, function space
    sfns = Shapefns()
    V = FunctionSpace(mesh,sfns)
    
    # rhs and exact
    x = V.dofpts()
    exact = exactfn(x)

    rhs = rhsfn(x)
    b = V.int_phi(rhs)

    # assemble stiffness matrix: u''+2*u'+u
    # integration by parts on first term introduces minus sign
    A = -V.int_phi_phi(derivative=[True,True]) \
        +2*V.int_phi_phi(derivative=[False,True]) \
        +V.int_phi_phi()

    # insert boundary conditions
    # left bndry u=1 (from exact solution)
    A[0,0] = 1.0
    A[0,1:] = 0.0
    b[0] = exact[0]
    # right bndry u=1 (from exact solution)
    A[-1,-1] = 1.0
    A[-1,0:-1] = 0.0
    b[-1] = exact[-1]

    # solve
    u = la.solve(A,b)

    # check
    print title," relative error=",la.norm(u-exact)/la.norm(exact)

verification("Case 1",5,rhsfn=lambda(x):0.0*x+1.0,exactfn=lambda(x):0.0*x+1.0)
verification("Case 2",5,rhsfn=lambda(x):x+2.0,exactfn=lambda(x):x)
verification("Case 3",5,rhsfn=lambda(x):x**2+4*x+2.0,exactfn=lambda(x):x**2)

# you must complete the code!
