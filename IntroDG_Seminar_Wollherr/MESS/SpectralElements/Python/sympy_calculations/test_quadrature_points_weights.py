#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Tests for quadrature points and weights.

:copyright:
    Martin van Driel (Martin@vanDriel.de), 2015
:license:
    GNU Lesser General Public License, Version 3
    (http://www.gnu.org/copyleft/lgpl.html)
"""
import numpy as np

from gauss_points_weights import *


def test_gauss_quadruature_points_weights():

    # reference data for n = 5
    ref_points = [-0.906179845938664, -0.538469310105683, 0.,
                  0.538469310105683, 0.906179845938664]
    ref_weights = [0.236926885056189, 0.478628670499366, 0.568888888888889,
                   0.478628670499366, 0.236926885056189]

    points, weights = gauss_quadruature_points_weights(5)
    np.testing.assert_allclose(points, ref_points, atol=1e-15)
    np.testing.assert_allclose(weights, ref_weights, atol=1e-15)


def test_gauss_lobatto_legendre_quadruature_points_weights():

    # reference data for n = 5
    ref_points = [-1., -21 ** 0.5 / 7., 0., 21 ** 0.5 / 7., 1.]
    ref_weights = [1./10., 49./90., 32./45., 49./90., 1./10.]

    points, weights = gauss_lobatto_legendre_quadruature_points_weights(5)
    np.testing.assert_allclose(points, ref_points, atol=1e-15)
    np.testing.assert_allclose(weights, ref_weights, atol=1e-15)
