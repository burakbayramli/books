#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Routines to compute quadrature points and weights.

:copyright:
    Martin van Driel (Martin@vanDriel.de), 2015
:license:
    GNU Lesser General Public License, Version 3
    (http://www.gnu.org/copyleft/lgpl.html)
"""
import numpy as np
import sympy as sp


def gauss_quadruature_points_weights(n=5):
    """
    compute Gaussian quadrature weights and points [-1, 1] using the sympy
    symbolic package and analytical definitions of these points

    somehow seems not to work for even numbers n >= 10, I assume this is a bug
    in sympy

    :param n: number of integration points (order + 1)
    :type n: integer

    :returns: tuple of two numpy arrays of floats containing the points and
        weights
    """
    x = sp.symbols('x')

    # Gauss points are defined as the roots of the Legendre Polynomials
    Pn = sp.legendre(n, x)

    # evaluate and sort
    pointsl = []
    for i in np.arange(n):
        p = sp.RootOf(Pn, i)
        pointsl.append(p.evalf())

    pointsl.sort()

    # weights are found through the derivative of the Legendre Polynomials
    dPn = sp.diff(Pn)

    # evaluate
    weightsl = []
    for p in pointsl:
        wi = 2. / ((1 - p ** 2) * dPn.subs(x, p) ** 2)
        weightsl.append(wi)

    return np.array(pointsl, dtype='float'), np.array(weightsl, dtype='float')


def gauss_lobatto_legendre_quadruature_points_weights(n=5):
    """
    compute Gauss-Lobatto-Legendre (GLL) quadrature weights and points [-1, 1]
    using the sympy symbolic package and analytical definitions of these points

    somehow seems not to work for even numbers n >= 10, I assume this is a bug
    in sympy

    :param n: number of integration points (order + 1)
    :type n: integer

    :returns: tuple of two numpy arrays of floats containing the points and
        weights
    """
    x = sp.symbols('x')

    # GLL points are defined as the roots of the derivative of the Legendre
    # Polynomials and include the boundaries
    dPn = sp.diff(sp.legendre(n-1, x))

    # evaluate and sort
    pointsl = []
    for i in np.arange(n):
        p = sp.RootOf((1 - x ** 2) * dPn, i)
        pointsl.append(p.evalf())

    pointsl.sort()

    # weights
    Pn = sp.legendre(n-1, x)
    weightsl = []
    for p in pointsl:
        if p == -1. or p == 1.:
            wi = 2. / (n * (n - 1))
        else:
            wi = 2. / (n * (n - 1) * Pn.subs(x, p) ** 2)
        weightsl.append(wi)

    return np.array(pointsl, dtype='float'), np.array(weightsl, dtype='float')
