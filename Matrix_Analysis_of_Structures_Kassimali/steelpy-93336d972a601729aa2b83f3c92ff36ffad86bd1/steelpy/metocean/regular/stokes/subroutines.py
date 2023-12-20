#
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
#
# Python stdlib imports
from array import array
from dataclasses import dataclass
#import math
from typing import NamedTuple

# package imports
import numpy as np

# 
def F(kd:float, H:float, T:float,
      Current:float, Current_criterion:int,
      C:array, n:int, D:array):
    """
    Evaluates dispersion relation - used in iterative solution for wavelength
    """
    pi = np.pi
    kH = kd * H
    e2 = 0.5 * kH * 0.5 * kH
    rhs = Current * np.sqrt(kd) - 2.0 * pi / T / np.sqrt(kd)
    CC = np.zeros(3)
    CC[0] = C[0]
    CC[2] = 0.0
    CC[1] = CC[2]
    if n >= 3:
        CC[1] = e2 * C[2]
    #
    if n >= 5:
        CC[2] = e2 * e2 * C[4]
    
    if Current_criterion == 2:
        if n >= 3:
            CC[1] += e2 * D[2] / kd
        
        if n >= 5:
            CC[2] += e2 * e2 * D[4] / kd
    #
    CC[1] = CC[0] + CC[1]
    CC[2] = CC[1] + CC[2]
    # return rhs+CC[2]; // Should this be current criterion rather than 2?
    return rhs + CC[Current_criterion]


#
#
def CDE(kd:float):
    """
    Calculate coefficient arrays C[], D[] and E[]
    """
    skd = np.sinh(kd)
    ckd = 1.0 / np.tanh(kd)
    tkd = np.tanh(kd)
    S = 1.0 / np.cosh(2.0 * kd)
    ss = [0, S]
    for i in range(2, 8 + 1):
        ss.append(ss[i - 1] * S)
    #
    t = [0, 1.0 - S]
    for i in range(2, 8 + 1):
        t.append(t[i - 1] * (1.0 - S))
    #
    C = np.zeros(5)
    D = np.zeros(5)
    E = np.zeros(5)
    C[0] = np.power(tkd, 0.5)
    C[2] = np.power(tkd, 0.5) * (2 + 7 * ss[2]) / (4 * t[2])
    C[4] = np.power(tkd, 0.5) * (4 + 32 * ss[1] - 116 * ss[2] - 400 * ss[3] - 71 * ss[4] + 146 * ss[5]) / (32 * t[5])
    D[2] = -np.power(ckd, 0.5) / 2
    D[4] = np.power(ckd, 0.5) * (2 + 4 * ss[1] + ss[2] + 2 * ss[3]) / (8 * t[3])
    E[2] = tkd * (2 + 2 * ss[1] + 5 * ss[2]) / (4 * t[2])
    E[4] = tkd * (8 + 12 * ss[1] - 152 * ss[2] - 308 * ss[3] - 42 * ss[4] + 77 * ss[5]) / (32 * t[5])
    return ckd, skd, ss, t, C, D, E
#
# 
def AB(skd:float, ss:list, t:list, n:int,
       z:array, e:array, C:array, kd:float, ckd:float):
    """
    Calculate coefficient arrays A[] and B[] and Fourier coefficients
    """
    BB = np.zeros((n + 1, n + 1))
    A = np.zeros((n + 1, n + 1))
    B = np.zeros(n + 1)
    Y = np.zeros(n + 1)
    #
    A[1][1] = 1 / skd
    A[2][2] = 3*ss[2] / (2*t[2])
    A[3][1] = (-4 - 20*ss[1] + 10*ss[2] - 13*ss[3]) / (8*skd*t[3])
    A[3][3] = (-2*ss[2] + 11*ss[3]) / (8*skd*t[3])
    A[4][2] = (12*ss[1] - 14*ss[2] - 264*ss[3] - 45*ss[4] - 13*ss[5]) / (24*t[5])
    A[4][4] = (10*ss[3] - 174*ss[4] + 291*ss[5] + 278*ss[6]) / (48*(3 + 2*ss[1])*t[5])
    A[5][1] = ((-1184 + 32*ss[1] + 13232*ss[2] + 21712*ss[3] + 20940*ss[4] 
                + 12554*ss[5] - 500*ss[6] - 3341*ss[7] - 670*ss[8]) 
               / (64*skd*(3 + 2*ss[1]) * (4 + ss[1])*t[6]))
    A[5][3] = ((4*ss[1] + 105*ss[2] + 198*ss[3] - 1376*ss[4] - 1302*ss[5] - 117*ss[6] + 58*ss[7]) 
               / (32*skd*(3 + 2*ss[1])*t[6]))
    A[5][5] = ((-6*ss[3] + 272*ss[4] - 1552*ss[5] + 852*ss[6] + 2029*ss[7] + 430*ss[8]) 
               / (64*skd*(3 + 2*ss[1])*(4 + ss[1])*t[6]))

    for i in range(1, n + 1):
        z[n + 10 + i] = 0.0
        jj = ((i + 1) % 2) + 1
        for j in range(jj, n, 2):
            z[n + 10 + i] += A[j][i] * e[j]
        #
        z[n + 10 + i] *= C[0] * np.cosh(i * kd)
        # Fourier coefficients
        B[i] = z[n + 10 + i]
    #
    BB[1][1] = 1.0
    BB[2][2] = ckd*(1 + 2*ss[1]) / (2*t[1])
    BB[3][1] = -3*(1 + 3*ss[1] + 3*ss[2] + 2*ss[3]) / (8*t[3])
    BB[3][3] = -BB[3][1]
    BB[4][2] = (ckd*(6 - 26*ss[1] - 182*ss[2] - 204*ss[3] - 25*ss[4] + 26*ss[5]) 
                / (6*(3 + 2*ss[1])*t[4]))
    BB[4][4] = (ckd*(24 + 92*ss[1] + 122*ss[2] + 66*ss[3] + 67*ss[4] + 34*ss[5]) 
                / (24*(3 + 2*ss[1])*t[4]))
    BB[5][3] = (9*(132 + 17*ss[1] - 2216*ss[2] - 5897*ss[3] - 6292*ss[4] 
                   - 2687*ss[5] + 194*ss[6] + 467*ss[7] + 82*ss[8]) 
                / (128*(3 + 2*ss[1])*(4 + ss[1])*t[6]))
    BB[5][5] = (5*(300 + 1579*ss[1] + 3176*ss[2] + 2949*ss[3] + 1188*ss[4] 
                   + 675*ss[5] + 1326*ss[6] + 827*ss[7] + 130*ss[8]) 
                / (384*(3 + 2*ss[1])*(4 + ss[1])*t[6]))
    BB[5][1] = -(BB[5][3] + BB[5][5])
    # Discrete Fourier transform of the surface elevations.
    for i in range(1, n + 1):
        Y[i] = 0.0
        j = ((i + 1) % 2) + 1
        while j <= n:
            Y[i] += BB[j][i] * e[j]
            j += 2
    #
    return Y, z, A, B
#