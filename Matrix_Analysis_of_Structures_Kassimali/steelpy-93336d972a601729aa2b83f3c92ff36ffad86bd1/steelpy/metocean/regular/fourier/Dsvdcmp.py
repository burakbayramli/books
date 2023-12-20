#
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
#
# Python stdlib imports
from array import array
#from dataclasses import dataclass
#import math
#from typing import NamedTuple, Tuple, Union, List, Dict

# package imports
from steelpy.metocean.regular.fourier.Dpythag import dpythag
#from steelpy.metocean.regular.operations.waveops import zeros

import numpy as np


def dsvdcmp(a: array, m: int, n: int, NP: int):
    """
    decomposition
    """
    w = np.zeros(NP + 1)
    v = np.zeros((NP + 1, NP + 1))
    rv1 = np.zeros(n + 1)
    # rv2 = [0]
    anorm = 0.0
    scale = 0.0
    g = 0.0
    for i in range(1, n + 1):
        l = i + 1
        rv1[i] = scale * g
        # rv2.append(scale * g)
        # scale = 0.0
        s = 0.0
        g = 0.0
        if i <= m:
            #scale = np.sum([np.abs(a[k][i]) for k in range(i, m + 1)])
            #if scale != 0:            
            try:
                1 / (scale := np.sum(np.abs(a[i: m + 1, i])))
                #
                a[i: m + 1, i] /= scale
                s = np.sum(np.power(a[i: m + 1, i], 2))
                #for k in range(i, m + 1):
                #    a[k][i] /= scale
                #    s += a[k][i] * a[k][i]
                # ss = sum([a[k][i] * a[k][i] for k in range(i, m+1)])
                f = a[i][i]
                g = np.copysign(np.sqrt(s), f) * -1.0
                h = f * g - s
                a[i][i] = f - g

                for j in range(l, n + 1):
                    s = np.sum(a[i: m + 1, i] * a[i: m + 1, j])
                    #s = sum([a[k][i] * a[k][j]
                    #         for k in range(i, m + 1)])
                    f = s / h
                    #
                    a[i: m + 1, j] += f * a[i: m + 1, i]
                    #for k in range(i, m + 1):
                    #    a[k][j] += f * a[k][i]
                #
                a[i: m + 1, i] *= scale
                #for k in range(i, m + 1):
                #    a[k][i] *= scale
            except ZeroDivisionError:
                pass
        #
        w[i] = scale * g
        # scale = 0.0
        s = 0.0
        g = 0.0
        if i <= m and i != n:
            #scale = sum([abs(a[i][k]) for k in range(l, n + 1)])
            #if scale != 0:
            try:
                1 / (scale := np.sum(np.abs(a[i, l: n + 1])))
                #
                a[i, l: n + 1] /= scale
                s = np.sum(np.power(a[i, l: n + 1], 2))
                #for k in range(l, n + 1):
                #    #a[i][k] /= scale
                #    s += a[i][k] * a[i][k]
                #
                f = a[i][l]
                g = np.copysign(np.sqrt(s), f) * -1.0
                h = f * g - s
                a[i][l] = f - g

                for k in range(l, n + 1):
                    try:
                        rv1[k] = a[i][k] / h
                    except ZeroDivisionError:
                        rv1[k] = 0
                #
                # rv1[l:] = [a[i][k] / h for k in range(l, n+1)
                #           if h != 0.0]
                #
                for j in range(l, m + 1):
                    s = np.sum(a[j, l: n + 1] * a[i, l: n + 1])
                    #s = np.sum([a[j][k] * a[i][k]
                    #            for k in range(l, n + 1)])
                    #
                    a[j, l: n + 1] += s * rv1[l: n + 1]
                    #for k in range(l, n + 1):
                    #    a[j][k] += s * rv1[k]
                #
                a[i, l: n + 1] *= scale
                #for k in range(l, n + 1):
                #    a[i][k] *= scale
            except ZeroDivisionError:
                pass
        #
        anorm = np.maximum(anorm, (np.abs(w[i]) + np.abs(rv1[i])))
    # Next i
    for i in range(n, 0, -1):
        if i < n:
            #if g != 0:
            try:
                1 / g
                #v[l: n + 1, i] = (a[i, l: n + 1] / a[i, l: n + 1]) / g
                for j in range(l, n + 1):
                    try:
                        v[j][i] = (a[i][j] / a[i][l]) / g
                    except ZeroDivisionError:
                        v[j][i] = 0
                #
                for j in range(l, n + 1):
                    s = np.sum(a[i, l: n + 1] * v[l: n + 1, j])
                    #s = sum([a[i][k] * v[k][j] for k in range(l, n + 1)])
                    v[l: n + 1, j] += s * v[l: n + 1, i]
                    #for k in range(l, n + 1):
                    #    v[k][j] += s * v[k][i]
            except ZeroDivisionError:
                #v[l: n + 1, i] = 0
                pass
            # TODO : is this necessary?
            v[i, l: n + 1] = 0.0
            v[l: n + 1, i] = 0.0            
            #for j in range(l, n + 1):
            #    v[i][j] = 0.0
            #    v[j][i] = 0.0
        #
        v[i][i] = 1.0
        g = rv1[i]
        l = i
    # 
    ii = np.minimum(m, n)
    for i in range(ii, 0, -1):
        l = i + 1
        g = w[i]
        #
        a[i, l: n + 1] = 0.0
        #for j in range(l, n + 1):
        #    a[i][j] = 0.0
        #
        #if g != 0:
        try:
            g = 1.0 / g
            for j in range(l, n + 1):
                s = np.sum(a[l: m + 1, i] * a[l: m + 1, j])
                #s = np.sum([a[k][i] * a[k][j]
                #            for k in range(l, m + 1)])
                f = (s / a[i][i]) * g
                #
                a[i: m + 1, j] += f * a[i: m + 1, i]
                #for k in range(i, m + 1):
                #    a[k][j] += f * a[k][i]
            #
            a[i: m + 1, i] *= g
            #for j in range(i, m + 1):
            #    a[j][i] *= g
        except ZeroDivisionError:
            a[i: m + 1, i] = 0.0
            #for j in range(i, m + 1):
            #    a[j][i] = 0.0
        #
        a[i][i] += 1
        # i -= 1
    # Loop
    for k in range(n, 0, -1):
        for its in range(1, 30 + 1):
            flag = 1
            for l in range(k, 0, -1):
                nm = l - 1
                if np.abs(rv1[l]) + anorm == anorm:
                    flag = 0
                    break

                if np.abs(w[nm]) + anorm == anorm:
                    break
            #
            if flag != 0:
                c = 0.0
                s = 1.0
                for i in range(l, k + 1):
                    f = s * rv1[i]
                    rv1[i] = c * rv1[i]
                    if np.abs(f) + anorm == anorm:
                        break
                    #
                    g = w[i]
                    h = dpythag(f, g)
                    w[i] = h
                    h = 1.0 / h
                    c = g * h
                    s = -f * h
                    for j in range(1, m + 1):
                        y = a[j][nm]
                        z = a[j][i]
                        a[j][nm] = y * c + z * s
                        a[j][i] = z * c - y * s
            #
            z = w[k]
            if l == k:
                if z < 0.0:
                    w[k] = -z
                    v[1: n + 1, k] = -1 * v[1: n + 1, k]
                    #for j in range(1, n + 1):
                    #    v[j][k] = -1 * v[j][k]
                break
            #
            if its == 50:
                print("no convergence in 30 dsvdcmp iterations")
            x = w[l]
            nm = k - 1
            y = w[nm]
            g = rv1[nm]
            h = rv1[k]
            f = (((y - z) * (y + z)
                  + (g - h) * (g + h)) / (2.0 * h * y))
            g = dpythag(f, 1.0)
            # 
            f = (((x - z) * (x + z)
                  + h * ((y / (f + np.copysign(g, f))) - h)) / x)
            s = 1.0
            c = s
            for j in range(l, nm + 1):
                i = j + 1
                g = rv1[i]
                y = w[i]
                h = s * g
                g = c * g
                z = dpythag(f, h)
                rv1[j] = z
                c = f / z
                s = h / z
                f = x * c + g * s
                g = g * c - x * s
                h = y * s
                y *= c
                for jj in range(1, n + 1):
                    x = v[jj][j]
                    z = v[jj][i]
                    v[jj][j] = x * c + z * s
                    v[jj][i] = z * c - x * s
                #
                z = dpythag(f, h)
                w[j] = z
                if z != 0:
                    z = 1.0 / z
                    c = f * z
                    s = h * z
                #
                f = c * g + s * y
                x = c * y - s * g
                for jj in range(1, m + 1):
                    y = a[jj][j]
                    z = a[jj][i]
                    a[jj][j] = y * c + z * s
                    a[jj][i] = z * c - y * s
            #
            rv1[l] = 0.0
            rv1[k] = f
            w[k] = x
    # End Sub
    return a, w, v


#
#
#
