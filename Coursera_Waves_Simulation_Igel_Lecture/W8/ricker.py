#!/usr/bin/env python
# -*- coding: utf-8 -*-
import math
import numpy as np


def ricker(dt, pt):
    """
    RICKER generate a ricker wavelet
    input (dt,period)
    """
    nt = int(2 * pt / dt)
    c = np.zeros(nt)
    t0 = pt / dt
    a_ricker = 4 / pt

    for it in range(0, nt):
        t = ((it + 1) - t0) * dt
        c[it] = -2 * a_ricker * t * math.exp(-(a_ricker * t) ** 2)

    return c
