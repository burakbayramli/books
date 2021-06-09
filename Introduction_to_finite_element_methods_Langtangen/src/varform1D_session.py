import sympy as sym

# Computing with Dirichlet conditions: -u''=2 and sines
x, L = sym.symbols('x L')
e_Galerkin = x*(L-x) - 8*L**2*sym.pi**(-3)*sym.sin(sym.pi*x/L)
e_colloc = x*(L-x) - 2*L**2*sym.pi**(-2)*sym.sin(sym.pi*x/L)

# Verify max error for x=L/2
dedx_Galerkin = sym.diff(e_Galerkin, x)
print(dedx_Galerkin.subs(x, L/2))
dedx_colloc = sym.diff(e_colloc, x)
print(dedx_colloc.subs(x, L/2))

# Compute max error: x=L/2, evaluate numerical, and simplify
print('Max error Galerkin/least.sq.:', \
      sym.simplify(e_Galerkin.subs(x, L/2).evalf(n=3)))
print('Max error colloc.:', \
      sym.simplify(e_colloc.subs(x, L/2).evalf(n=3)))
import sys
#sys.exit(0)


# Computing with Neumann and Dirichlet conditions: -u''=2
x, C, D = sym.symbols('x C D')
i, j = sym.symbols('i j', integer=True)

integrand = (i+1)*(j+1)*(1-x)**(i+j)
A_ij = sym.integrate(integrand, (x, 0, 1))
A_ij = sym.simplify(A_ij)
print(A_ij)
psi_i = (1-x)**(i+1)
integrand = 2*psi_i - D*(i+1)*(1-x)**i
b_i = sym.integrate(integrand, (x, 0, 1)) - C*psi_i.subs(x, 0)
b_i = sym.factor(sym.simplify(b_i))
print(b_i)
print(sym.expand(2 - (2+i)*(D+C)))

# Solving model2 problem with f(x) and fe1D.py
from u_xx_f_sympy import model2, x, C, D, L
m = 2
u = model2(x**m, L, C, D)
print(u)
#u_exact = lambda x: D + C*(x-L) + (1./6)*(L**3 - x**3)
u_exact = sym.lambdify([x, C, D, L], u)

import numpy as np
from fe1D import finite_element1D_naive, mesh_uniform
# Override C, D and L with numeric values
C = 5
D = 2
L = 4

d = 1

vertices, cells, dof_map = mesh_uniform(
    N_e=2, d=d, Omega=[0,L], symbolic=False)
vertices[1] = 3
essbc = {}
essbc[dof_map[-1][-1]] = D

c, A, b, timing = finite_element1D_naive(
    vertices, cells, dof_map,
    essbc,
    ilhs=lambda e, phi, r, s, X, x, h:
    phi[1][r](X, h)*phi[1][s](X, h),
    irhs=lambda e, phi, r, X, x, h:
    x**m*phi[0][r](X),
    blhs=lambda e, phi, r, s, X, x, h: 0,
    brhs=lambda e, phi, r, X, x, h:
    -C*phi[0][r](-1) if e == 0 else 0,
    intrule='GaussLegendre',
    verbose=False,
    )

# Visualize
from fe1D import u_glob
x, u, nodes = u_glob(c, cells, vertices, dof_map)
u_e = u_exact(x, C, D, L)
print(u_exact(nodes, C, D, L) - c)  # difference at the nodes
import matplotlib.pyplot as plt
plt.plot(x, u, 'b-', x, u_e, 'r--')
plt.legend(['finite elements, d=%d' %d, 'exact'], loc='upper left')
plt.savefig('tmp.png'); plt.savefig('tmp.pdf')
plt.show()


