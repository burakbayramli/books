#!/usr/bin/env python
# -*- coding: utf-8 -*-


def lagrange(N, i, x):
    """
    Function to calculate  Lagrange polynomial for order N and polynomial
    i[0, N] at location x.
    """
    from gll import gll

    [xi, weights] = gll(N)
    fac = 1
    for j in range(-1, N):
        if j != i:
            fac = fac * ((x - xi[j + 1]) / (xi[i + 1] - xi[j + 1]))
    return fac
