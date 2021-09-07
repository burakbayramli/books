#!/usr/bin/env python
# -*- coding: utf-8 -*-
import numpy as np

from gll import gll
from lagrange import lagrange
from legendre import legendre


def lagrange1st(N):
    """
    # Calculation of 1st derivatives of Lagrange polynomials
    # at GLL collocation points
    # out = legendre1st(N)
    # out is a matrix with columns -> GLL nodes
    #                        rows -> order
    """
    out = np.zeros([N+1, N+1])

    [xi, w] = gll(N)

    # initialize dij matrix (see Funaro 1993 or Diploma thesis Bernhard
    # Schuberth)
    d = np.zeros([N + 1, N + 1])

    for i in range(-1, N):
        for j in range(-1, N):
            if i != j:
                d[i + 1, j + 1] = legendre(N, xi[i + 1]) / \
                    legendre(N, xi[j + 1]) * 1.0 / (xi[i + 1] - xi[j + 1])
            if i == -1:
                if j == -1:
                    d[i + 1, j + 1] = -1.0 / 4.0 * N * (N + 1)
            if i == N-1:
                if j == N-1:
                    d[i + 1, j + 1] = 1.0 / 4.0 * N * (N + 1)

    # Calculate matrix with 1st derivatives of Lagrange polynomials
    for n in range(-1, N):
        for i in range(-1, N):
            sum = 0
            for j in range(-1, N):
                sum = sum + d[i + 1, j + 1] * lagrange(N, n, xi[j + 1])

            out[n + 1, i + 1] = sum
    return(out)
