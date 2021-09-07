#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Tests for interpolating polynomial bases and there derivatives.

:copyright:
    Martin van Driel (Martin@vanDriel.de), 2015
:license:
    GNU Lesser General Public License, Version 3
    (http://www.gnu.org/copyleft/lgpl.html)
"""
import numpy as np

from basis_polynomials import *
from quadrature_points_weights import *


def test_lagrange_basis_derivative_matrix():

    # reference data for n = 5
    ref_derivative = np.array([
        [-5.0000000000000000, 6.7565024887242409, -2.6666666666666665,
         1.4101641779424268, -0.50000000000000000],
        [-1.2409902530309833, 0.0000000000000000, 1.7457431218879389,
         -0.76376261582597338, 0.25900974696901713],
        [0.37500000000000000, -1.3365845776954530, 0.0000000000000000,
         1.3365845776954532, -0.37500000000000000],
        [-0.25900974696901718, 0.76376261582597338,
         -1.7457431218879396, 0.0000000000000000, 1.2409902530309824],
        [0.50000000000000000, -1.4101641779424265, 2.6666666666666665,
         -6.7565024887242382, 5.000000000000000]])

    points = gauss_lobatto_legendre_quadruature_points_weights(5)[0]
    derivative = lagrange_basis_derivative_matrix(points)
    np.testing.assert_allclose(derivative, ref_derivative, atol=1e-15)
