"""
Examples on approximating functions by global basis functions,
using the approx2D.py module.
"""
from approx2D import *
import sympy as sym
import sys
x, y = sym.symbols('x y')


def sines(x, y, Nx, Ny):
    return [sym.sin(sym.pi*(i+1)*x)*sym.sin(sym.pi*(j+1)*y)
            for i in range(Nx+1) for j in range(Ny+1)]

def taylor(x, y, Nx, Ny):
    return [x**i*y**j for i in range(Nx+1) for j in range(Ny+1)]


# ----------------------------------------------------------------------

def run_linear():
    f = (1+x**2)*(1+2*y**2)
    psi = taylor(x, y, 1, 1)
    print(psi)
    Omega = [[0, 2], [0, 2]]
    u = least_squares(f, psi, Omega)
    comparison_plot(f, u, Omega, plotfile='approx2D_bilinear')
    print('\n\n**** Include second order terms:')
    psi = taylor(x, y, 2, 2)
    u = least_squares(f, psi, Omega)


if __name__ == '__main__':
    # Run from command line:
    # python ex_approx2D.py run_linear
    cmd = sys.argv[1]
    args = ''
    if len(sys.argv) > 2:
        args = ','.join(sys.argv[2:])
    eval('{0}({1})'.format(cmd, args))
