#!/usr/bin/env python
# -*- coding: utf-8 -*-
import numpy as np


def flux(alpha, u, N, ne, mu):
    """
    Calculates the flux between two boundary sides of connected elements for
    element i

    General formula: Flux matrix du
    """
    # impose boundary conditions at x=0 and x=end
    ubd1 = 0
    ubd2 = 0

    # for every element we have 2 faces to other elements (left and right)
    du = np.zeros((N + 1, ne))
    for i in range(0, ne):
        # Left boundary
        if i == 0:
            # Left flux.
            du[0, i] = mu / 2.0 * (u[0, i] + ubd1) + (1.0 - alpha) * \
                abs(mu) / 2.0 * (ubd1 - u[0, i])
            # Right flux.
            du[N, i] = -mu / 2.0 * (u[N, i] + u[0, i + 1]) - (1.0 - alpha) \
                * abs(mu) / 2.0 * (u[N, i] - u[0, i + 1])
        # Right boundary
        elif i == ne-1:
            du[0, i] = mu * (u[0, i] + u[N, i - 1]) / 2.0 + (1.0 - alpha) * \
                abs(mu) / 2.0 * (-u[0, i] + u[N, i - 1])
            du[N, i] = -mu * (u[N, i] + ubd2) / 2.0 - (1.0 - alpha) * \
                abs(mu) / 2.0 * (u[N, i] - ubd2)
        # In the middle of the domain.
        else:
            du[0, i] = mu * (u[0, i] + u[N, i - 1]) / 2.0 + (1.0 - alpha) * \
                abs(mu) / 2.0 * (-u[0, i] + u[N, i - 1])
            du[N, i] = -mu * (u[N, i] + u[0, i + 1]) / 2.0 - (1.0 - alpha) * \
                abs(mu) / 2.0 * (u[N, i] - u[0, i + 1])

    return du
