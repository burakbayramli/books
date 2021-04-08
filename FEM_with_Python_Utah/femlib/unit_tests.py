import os
import sys
import unittest
import argparse
import numpy as np
from math import fabs, sqrt
from sympy import symbols, Matrix, diff, lambdify, integrate
from sympy.parsing.sympy_parser import parse_expr, standard_transformations

from mesh import Mesh
from element import LinearElement, Element
from funspace import FunctionSpace
from numerix import areclose, allclose, norm, relerr
from ifem import simplemesh, solve_system

# constants used in many tests
x = symbols('x')
N = Matrix(2, 1, [1 - x, x])
dN = Matrix(2, 1, [-1, 1])


def test_linear_element():
    '''Test the implementation of the linear element with simple integrals'''
    dx = 5.
    num_elem = 5
    mesh = Mesh(type='uniform', ox=0., lx=dx, nx=num_elem)

    elem_num = 1
    connect = mesh.connectivity(elem_num)
    vertices = mesh.coordinates(connect)
    elem = LinearElement(elem_num, connect, vertices)

    # --- test some integrals
    ex = lambda x: x
    ex2 = lambda x: x ** 2

    # Integrate[phi]
    ans = elem.integrate()
    exact = integrate(N, (x, 0, 1))
    assert areclose(ans, exact)

    # Integrate[dphi]
    ans = elem.integrate(derivative=True)
    exact = integrate(dN, (x, 0, 1))
    assert areclose(ans, exact)

    # Integrate[x phi]
    ans = elem.integrate(ex)
    exact = integrate(x * N, (x, 0, 1))
    assert areclose(ans, exact)

    # Integrate[x dphi]
    ans = elem.integrate(ex, derivative=True)
    exact = integrate(x * dN, (x, 0, 1))
    assert areclose(ans, exact)

    # Integrate[x x phi]
    ans = elem.integrate(ex, ex)
    exact = integrate(x * x * N, (x, 0, 1))
    assert areclose(ans, exact)

    # Integrate[x x dphi]
    ans = elem.integrate(ex,ex,derivative=True)
    exact = integrate(x*x*dN,(x,0,1))
    assert areclose(ans, exact)

def test_funspace_1():
    '''Test of FunctionSpace.int_phi made up of linear elements'''
    ox, dx, nx = 0., 10., 5
    mesh = Mesh(type='uniform', lx=dx, nx=nx, block='B1')
    V = FunctionSpace(mesh, {'B1': Element(type='link2')})

    # basic properties
    assert V.num_dof == (nx + 1)
    assert V.size == nx
    X = np.linspace(ox, dx, nx+1)
    assert allclose(V.X, X)

    # Integrate[1, {x, ox, lx}]
    f = lambda x: 1
    ans = np.sum(V.int_phi(f))
    exact = V.X[-1]
    assert areclose(ans, exact)

    # Integrate[x, {x, ox, lx}]
    f = lambda x: x
    ans = np.sum(V.int_phi(f))
    exact = V.X[-1] ** 2 / 2.
    assert areclose(ans, exact)

    # Integrate[x**2, {x, ox, lx}]
    f = lambda x: x * x
    ans = np.sum(V.int_phi(f))
    exact = V.X[-1] ** 3 / 3.
    assert areclose(ans, exact)

    # Integrate[x**3, {x, ox, lx}]
    f = lambda x: x * x * x
    ans = np.sum(V.int_phi(f))
    exact = V.X[-1] ** 4 / 4.
    assert areclose(ans, exact)

    # Integrate[x**4, {x, ox, lx}]
    f = lambda x: x * x * x * x
    ans = np.sum(V.int_phi(f))
    exact = V.X[-1] ** 5 / 5.
    assert areclose(ans, exact, tol=1.5)

def test_funspace_2():
    '''Test of FunctionSpace.int_phi_phi made up of linear elements, basic
    integration

    '''
    ox, dx, nx = 0., 1., 1
    mesh = Mesh(type='uniform', lx=dx, nx=nx, block='B1')
    V = FunctionSpace(mesh, {'B1': Element(type='link2')})

    # basic properties
    assert V.num_dof == (nx + 1)
    assert V.size == nx
    X = np.linspace(ox, dx, nx+1)
    assert allclose(V.X, X)

    # Integrate[N(x) N(x) {x, 0, 1}]
    fun = lambda x: x
    f = lambda i, j: integrate(fun(x) * N[i] * N[j], (x, ox, dx))
    ans = V.int_phi_phi(fun=fun, derivative=(False, False))
    exact = Matrix(2, 2, lambda i, j: f(i,j))
    assert areclose(exact, ans)

    # Trivial check with coefficient
    fun = lambda x: 1.
    a1 = V.int_phi_phi()
    a2 = V.int_phi_phi(fun=fun)
    assert areclose(a1, a2)

def test_funspace_3():
    '''Test of FunctionSpace.int_phi_phi made up of linear elements, Laplace
    matrix integration

    '''
    ox, dx, nx = 0., 1., 1
    mesh = Mesh(type='uniform', lx=dx, nx=nx, block='B1')
    V = FunctionSpace(mesh, {'B1': Element(type='link2')})

    # Integrate[N'(x) N'(x) {x, 0, 1}]
    f = lambda i, j: integrate(dN[i] * dN[j], (x, ox, dx))
    ans = V.int_phi_phi(derivative=(True, True))
    exact = Matrix(2, 2, lambda i, j: f(i,j))
    assert areclose(exact, ans)

def test_funspace_4():
    '''Test of FunctionSpace.int_phi_phi made up of linear elements, Laplace
    matrix multiply

    '''
    ox, dx, nx = 0., 1., 10
    mesh = Mesh(type='uniform', lx=dx, nx=nx, block='B1')
    V = FunctionSpace(mesh, {'B1': Element(type='link2')})
    C = V.int_phi_phi(derivative=(True, True))

    sol = np.ones(V.num_dof)
    b = np.dot(C, sol)
    assert norm(b) < 1.e-12

def test_funspace_5():
    '''Test of FunctionSpace.int_phi_phi made up of linear elements, Laplace
    norm check.  Solution = x

    '''
    ox, dx, nx = 0., 1., 10
    mesh = Mesh(type='uniform', lx=dx, nx=nx, block='B1')
    V = FunctionSpace(mesh, {'B1': Element(type='link2')})
    C = V.int_phi_phi(derivative=(True, True))

    sol = V.X
    b = np.dot(C, sol)
    rhs = V.int_phi(lambda x: 0.)
    # natural b.c. not satisfied, don't check them
    rhs[0] = -b[0]
    rhs[-1] = -b[-1]
    assert norm(rhs + b) < 1.e-12

def test_funspace_6():
    '''Test of FunctionSpace.int_phi_phi made up of linear elements, Laplace
    norm check.  Solution = x^2

    '''
    ox, dx, nx = 0., 1., 10
    mesh = Mesh(type='uniform', lx=dx, nx=nx, block='B1')
    V = FunctionSpace(mesh, {'B1': Element(type='link2')})
    C = V.int_phi_phi(derivative=(True, True))

    sol = V.X ** 2
    b = np.dot(C, sol)
    rhs = V.int_phi(lambda x: 2.)
    # natural b.c. not satisfied on right, don't check it
    rhs[-1] = -b[-1]
    assert norm(rhs + b) < 1.e-12

def test_funspace_7():
    '''Test of FunctionSpace.int_phi_phi made up of linear elements, mixed
    matrix integration

    '''
    ox, dx, nx = 0., 1., 1
    mesh = Mesh(type='uniform', lx=dx, nx=nx, block='B1')
    V = FunctionSpace(mesh, {'B1': Element(type='link2')})

    # Integrate[N(x) N'(x) {x, 0, 1}]
    f = lambda i, j: integrate(N[i] * dN[j], (x, ox, dx))
    ans = V.int_phi_phi(derivative=(False, True))
    exact = Matrix(2, 2, lambda i, j: f(i,j))
    assert areclose(exact, ans)

    # Integrate[N'(x) N(x) {x, 0, 1}]
    f = lambda i, j: integrate(dN[i] * N[j], (x, ox, dx))
    ans = V.int_phi_phi(derivative=(True, False))
    exact = Matrix(2, 2, lambda i, j: f(i,j))
    assert areclose(exact, ans)

    return

def test_funspace_8():
    '''Testing of FunctionSpace.int_phi_phi, MMS'''
    ox, dx, nx = 0., 10., 10
    mesh = Mesh(type='uniform', lx=dx, nx=nx, block='B1')
    V = FunctionSpace(mesh, {'B1': Element(type='link2')})

    rhs = np.random.rand(V.num_dof)
    A = V.int_phi_phi()
    f = lambda x: np.interp(x, V.X, rhs)
    b = V.int_phi(f)
    assert areclose(np.dot(A, rhs), b)
    return

    # check eq \int \phi \phi * u = 1 gives 1 back (homog Neumann b.c.)
    # generate matrix by assembling $A_{ij}=\int\phi_i\phi_j$

def test_funspace_9():
    '''Testing of FunctionSpace.int_phi_phi, mixed integration MMS'''
    ox, dx, nx = 0., 10., 10
    mesh = Mesh(type='uniform', lx=dx, nx=nx, block='B1')
    V = FunctionSpace(mesh, {'B1': Element(type='link2')})

    D = V.int_phi_phi(derivative=(False,True))
    sol = np.ones(V.num_dof)
    b = np.dot(D, sol)
    # Norm check (rhs d/dx + Neumann, const soln)
    assert norm(b) < 1.e-12

    D[0, 0] = 1.0
    D[0, 1:] = 0.0
    D[-1, -1] = 1.0
    D[-1, 0:-1] = 0.0
    sol = V.X
    b = np.dot(D, sol)
    rhs = V.int_phi(lambda x: 1)
    rhs[0] = sol[0]
    rhs[-1] = sol[-1]
    # norm check (d/dx+Dirichlet sol=x)
    assert norm(rhs - b) < 1.e-12

def test_mesh():
    '''Basic test of the Mesh factory method and mesh extending'''
    ox, dx, nx = 0., 10., 5
    mesh = Mesh(type='uniform', ox=ox, lx=dx, nx=nx, block='B1')
    assert mesh.num_elem == nx
    assert mesh.boundary_nodes == [1, nx+1]
    assert np.allclose(mesh.boundary, [ox, dx])
    # verify extending the mesh
    dxb, nb = 4., 2
    mesh.extend(dxb, nb, block='B2')
    assert mesh.num_elem == nx + nb
    assert mesh.boundary_nodes == [1, nx+nb+1]
    assert np.allclose(mesh.boundary, [0., dx+dxb])
    assert len(mesh.nodes) == len(mesh.vertices)

def test_solve_system():
    '''Test simple ifem solver'''
    n = 1000
    offset = 0
    xa, xb = 0., 1.
    nodes, vertices, elements, connect = simplemesh(xa, xb, n)

    A, E, P = 1., 100., 10.
    k = np.zeros(n)
    for (el, econn) in enumerate(connect):
        i, j = nodes.index(econn[0]), nodes.index(econn[1])
        k[el] = A * E / (vertices[j] - vertices[i])

    delta = .05
    bcs = [(nodes[0],'x', delta)]
    cfs = [(nodes[-1], 'x', P)]
    u, r = solve_system(nodes, vertices, elements, connect, k, bcs, cfs)

    # check the error
    K = A * E / (xb - xa)
    d = P / K + delta
    assert areclose(d, u[-1])
    assert areclose(P, -r[0])
    assert allclose(r[1:], 0.)
