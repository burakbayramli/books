#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Routines to compute interpolating polynomial bases and there derivatives.

:copyright:
    Martin van Driel (Martin@vanDriel.de), 2015
:license:
    GNU Lesser General Public License, Version 3
    (http://www.gnu.org/copyleft/lgpl.html)
"""
import numpy as np
import sympy as sp


def lagrange_basis_polynomials(points):
    """
    compute Lagrange basis polynomials

    :param points: interpolation points
    :type points: list of floats

    :returns: list of sympy expressions containing the Lagrange basis
        polynomials
    """

    n = len(points)
    x = sp.symbols('x')
    polynomials = []
    for j in np.arange(n):
        lj = 1
        for m in np.arange(n):
            if m == j:
                continue

            lj *= (x - points[m]) / (points[j] - points[m])

        polynomials.append(lj)
    return polynomials


def lagrange_basis_derivative_matrix(points):
    """
    compute derivatives of Lagrange basis polynomials

    D(i,j) = d/dx l_j (x_i)

    :param points: interpolation points
    :type points: list of floats

    :returns: nupy array containing derivatives evaluated at the interpolation
        points
    """

    n = len(points)
    x = sp.symbols('x')
    polys = lagrange_basis_polynomials(points)
    derivative_matrix = np.zeros((n, n))

    for i in np.arange(n):
        for j in np.arange(n):
            derivative_matrix[i, j] = sp.diff(polys[j]).subs(x, points[i])

    return derivative_matrix
