# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
#
# Python stdlib imports
from array import array
from dataclasses import dataclass
#import cmath
#import math
#from typing import NamedTuple, Tuple, Union, List, Dict

# package imports
from steelpy.metocean.regular.fourier.solve import solver
#from steelpy.metocean.regular.operations.waveops import zeros
import numpy as np

#
# SUBROUTINES FOR FOURIER APPROXIMATION METHOD
#
# **************************************************
# CALCULATE INITIAL SOLUTION FROM LINEAR WAVE THEORY
# **************************************************
#


def initial(height: float, Hoverd: float,
            current: float, current_criterion: int,
            num: int, n: int, is_finite: bool, case: str):
    """ """
    pi = np.pi
    sol = np.zeros((num + 1, 2 + 1))
    z = np.zeros(num + 1)
    cosa = np.zeros(2 * n + 1)
    sina = np.zeros(2 * n + 1)
    case = case.lower()
    #
    if is_finite:
        if case == "period":
            sigma = 2. * pi * np.sqrt(height / Hoverd)
            # Fenton and McKee (1990) used in 2018
            z[1] = sigma * sigma / np.power(np.tanh(np.power(sigma, 1.5)), 2. / 3.)
            # That is simpler and replaces the procedure used since Fenton (1988), p360 & 365, based on Eckart's
            # solution plus a single pass of Newton's method, which is slightly more accurate than Fenton & McKee
            # for a small wave and zero Eulerian current. However usually we have finite wave height and a
            # current of some sort, and we cannot say which is more accurate. The method appears here, commented out:
            #
            # double a, b, t
            # a=4.*pi*pi*height/Hoverd
            # b=a/math.sqrt(math.tanh(a))
            # t=math.tanh(b)
            # z[1]=b+(a-b*t)/(t+b*(1.-t*t))
            #
        else:
            z[1] = 2. * pi * height / Hoverd

        z[2] = z[1] * Hoverd
        z[4] = np.sqrt(np.tanh(z[1]))
        z[3] = 2. * pi / z[4]

    else:  # Is_deep
        z[1] = 1.
        if case == "period":
            z[2] = 4 * pi * pi * height
        else:
            z[2] = 2 * pi * height
        z[3] = 2 * pi
        z[4] = 1.
    #
    if is_finite:
        if current_criterion == 1:
            z[5] = current * np.sqrt(z[2])
            z[6] = 0.
        else:
            z[6] = current * np.sqrt(z[2])
            z[5] = 0.
    else:  # Is_deep -- Much easier than four different cases
        z[5] = 0.
        z[6] = 0.

    z[7] = z[4]
    z[8] = 0.
    z[9] = 0.5 * z[7] * z[7]
    cosa[0] = 1.
    sina[0] = 0.
    z[10] = 0.5 * z[2]

    for i in range(1, n + 1):
        cosa[i] = np.cos(i * pi / n)
        cosa[i + n] = np.cos((i + n) * pi / n)
        sina[i] = np.sin(i * pi / n)
        sina[i + n] = np.sin((i + n) * pi / n)
        z[n + i + 10] = 0.
        z[i + 10] = 0.5 * z[2] * cosa[i]
    #
    z[n + 11] = 0.5 * z[2] / z[7]

    # This setting to zero of sol[i][1] is to give the very initial solution
    # that of a wave of zero height, used for extrapolation
    sol[1:10, 1] = z[1:10]
    #for i in range(1, 9 + 1):
    #    sol[i][1] = z[i]
    #
    #for i in range(10, num + 1):
    #    sol[i][1] = 0.
    #
    return sol, z, cosa, sina
#
#
#   EVALUATION OF EQUATIONS.
#


def eqns(height: float, Hoverd: float,
         z: array, cosa: array, sina: array,
         num: int, n: int, current: float,
         current_criterion: int, is_finite: bool, case: str):
    """
    EVALUATION OF EQUATIONS
    """
    rhs = np.zeros(num + 1)
    coeff = np.zeros(n + 1)
    Tanh = np.zeros(n + 1)
    pi = np.pi
    case = case.lower()
    rhs[1] = 0.
    if is_finite:
        rhs[1] = z[2] - z[1] * Hoverd
    # else: #is_deep:
    #    rhs[1] = 0.

    if case == "period":
        rhs[2] = z[2] - height * z[3] * z[3]
    else:  # iff(Case,Wavelength)
        rhs[2] = z[2] - 2. * pi * height

    rhs[3] = z[4] * z[3] - pi - pi
    rhs[4] = z[5] + z[7] - z[4]
    rhs[5] = z[1] * (z[6] + z[7] - z[4]) - z[8]
    #
    coeff[1: n + 1] = z[n + 10 + 1: n + 10 + n + 1]
    if is_finite:
        for i in range(1, n + 1):
            Tanh[i] = np.tanh(i * z[1])
        
    #for i in range(1, n + 1):
    #    #coeff[i] = z[n + i + 10]
    #    if is_finite:
    #        Tanh[i] = np.tanh(i * z[1])
    #
    # Correction made 20.5.2013, z[2] changed to z[1]
    if is_finite:
        #rhs[6]=z[current_criterion+4]-current* math.sqrt(z[1])
        rhs[6] = z[current_criterion + 4] - current * np.sqrt(z[1]).real
    else:  # Is_deep
        if case == "period":
            rhs[6] = z[current_criterion + 4] - current * z[3]
        else:  # if (Case,Wavelength):
            rhs[6] = z[current_criterion + 4] - current
    #
    #rhs[7] = z[10] + z[n + 10]
    rhs[7] = z[10] + z[n + 10] + np.sum(2 * z[10 + 1: 10 + n])
    #rhs[7] += np.sum(2 * z[10 + 1: 10+n])
    #rhs[7] += np.sum([2 * z[10 + i] for i in range(1, n)])
    #for i in range(1, n):  # -1
    #    rhs[7] = rhs[7] + z[10 + i] + z[10 + i]

    rhs[8] = z[10] - z[n + 10] - z[2]
    #
    if is_finite:
        for m in range(n + 1):
            psi = 0.
            u = 0.
            v = 0.
            for j in range(1, n + 1):
                nm = (m * j) % (n + n)
                #
                e = np.exp(j * (z[10 + m]))
                s = 0.5 * (e - 1. / e)
                c = 0.5 * (e + 1. / e)
                psi += coeff[j] * (s + c * Tanh[j]) * cosa[nm]
                u += j * coeff[j] * (c + s * Tanh[j]) * cosa[nm]
                v += j * coeff[j] * (s + c * Tanh[j]) * sina[nm]
            rhs[m + 9] = psi - z[8] - z[7] * z[m + 10]
            rhs[n + m + 10] = 0.5 * (np.power((-z[7] + u), 2.) + v * v) + z[m + 10] - z[9]
    
    else: # Is_deep
        for m in range(n + 1):
            psi = 0.
            u = 0.
            v = 0.
            for j in range(1, n + 1):
                nm = (m * j) % (n + n)
                #
                e = np.exp(j * (z[10 + m]))
                psi += coeff[j] * e * cosa[nm]
                u += j * coeff[j] * e * cosa[nm]
                v += j * coeff[j] * e * sina[nm]
            rhs[m + 9] = psi - z[8] - z[7] * z[m + 10]
            rhs[n + m + 10] = 0.5 * (np.power((-z[7] + u), 2.) + v * v) + z[m + 10] - z[9]
    
    #for m in range(n + 1):
    #    psi = 0.
    #    u = 0.
    #    v = 0.
    #    for j in range(1, n + 1):
    #        nm = (m * j) % (n + n)
    #        if is_finite:
    #            e = np.exp(j * (z[10 + m]))
    #            s = 0.5 * (e - 1. / e)
    #            c = 0.5 * (e + 1. / e)
    #            psi += coeff[j] * (s + c * Tanh[j]) * cosa[nm]
    #            u += j * coeff[j] * (c + s * Tanh[j]) * cosa[nm]
    #            v += j * coeff[j] * (s + c * Tanh[j]) * sina[nm]
    #        else:  # Is_deep
    #            e = np.exp(j * (z[10 + m]))
    #            psi += coeff[j] * e * cosa[nm]
    #            u += j * coeff[j] * e * cosa[nm]
    #            v += j * coeff[j] * e * sina[nm]
    #
    #    rhs[m + 9] = psi - z[8] - z[7] * z[m + 10]
    #    rhs[n + m + 10] = 0.5 * (np.power((-z[7] + u), 2.) + v * v) + z[m + 10] - z[9]
    #
    s = np.sum(rhs[1: num + 1] * rhs[1: num + 1])
    #s = np.sum([rhs[j] * rhs[j] for j in range(1, num + 1)])
    return s, rhs, Tanh
#
#
# **************************************************
# SET UP JACOBIAN MATRIX AND SOLVE MATRIX EQUATION
# **************************************************
#


def Newton(height, Hoverd, z, cosa, sina, num, n, current,
           current_criterion, is_finite, case):
    """
    SET UP JACOBIAN MATRIX AND SOLVE MATRIX EQUATION
    """
    case = case.lower()
    s, rhs1, Tanh = eqns(height, Hoverd, z, cosa, sina, num, n, current,
                         current_criterion, is_finite, case)

    rhs = np.zeros(num + 1)
    a = np.zeros((num + 1, num + 1))

    for i in range(1, num + 1):
        h = 0.01 * z[i]
        if np.abs(z[i]) < 1.e-4:
            h = 1.e-5
        z[i] += h
        # Eqns(rhs2)
        s, rhs2, Tanh = eqns(height, Hoverd, z, cosa, sina, num, n, current,
                             current_criterion, is_finite, case)
        z[i] -= h
        rhs[i] -= rhs1[i]
        #
        a[1: num + 1, i] = (rhs2[1: num + 1] - rhs1[1: num + 1]) / h
        #for j in range(1, num + 1):
        #    a[j][i] = (rhs2[j] - rhs1[j]) / h
    #
    #
    # **************************************************
    # SOLVE MATRIX EQUATION
    # **************************************************
    #
    x, a = solver(a, rhs, num, num, num)
    #
    z[1: num + 1] += x[1: num + 1]
    #for i in range(1, num + 1):
    #    z[i] += x[i]
    #
    sumt = np.sum(np.abs(x[10: n + 10 + 1]))/n
    #sumt = np.sum([abs(x[i]) for i in range(10, n + 10 + 1)]) / n
    return z, sumt, Tanh
#
