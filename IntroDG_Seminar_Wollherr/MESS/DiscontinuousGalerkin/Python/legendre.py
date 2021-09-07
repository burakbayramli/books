#!/usr/bin/env python
# -*- coding: utf-8 -*-
import numpy as np


def legendre(N, x):
    """
    Returns the value of Legendre Polynomial P_N(x) at position x[-1, 1].
    """
    P = np.zeros(2 * N)

    if N == 0:
        P[0] = 1
    elif N == 1:
        P[1] = x
    else:
        P[0] = 1
        P[1] = x
    for i in range(2, N + 1):
        P[i] = (1.0 / float(i)) * ((2 * i - 1) * x * P[i - 1] - (i - 1) *
                                   P[i - 2])

    return(P[N])
