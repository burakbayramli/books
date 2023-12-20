#
# Copyright (c) 2009-2023 steelpy
#
# Python stdlib imports
from __future__ import annotations
#from array import array
import math

# package imports
#import matplotlib.pyplot as plt
#import numpy as np

#
#
def title_block(is_finite, c_type, current, z):
    """
    PRINT OUT TITLE BLOCK
    """
    #
    #  Highest wave - eqn (32) of Fenton (1990)
    #
    pi = math.pi
    L = 2 * pi / z[1]
    Highest = ((0.0077829 * L * L * L + 0.0095721 * L * L + 0.141063 * L)
               / (0.0093407 * L * L * L + 0.0317567 * L * L + 0.078834 * L + 1))
    #
    current_name = {1:"Euler", 2:"Stokes"}
    #
    if is_finite:
        #print("# Height/Depth: {:6.3f}".format(MaxH))
        # print("# {:}".format(title))
        print("# ---------------------------------------------------------")
        print("# Height/Depth: {:7.4f}".format(z[2] / z[1]))
        print('# (A height of{:3.0f}% of the max of H/d={:1.3f} for this length)'
              .format(z[2] / z[1] / Highest * 100.0, Highest))        
        print("# Length/Depth: {:7.4f}".format(2 * math.pi / z[1]))
        print("# Dimensionless Period T*sqrt(g/d): {:7.4f}"
              .format(z[3] / math.sqrt(z[1])))
    else:
        print("# Height/Length: {:6.3f}, {:3.4}% of the maximum of H/L = {:6.3f}"
                .format(z[2]/2/pi,(z[2]/2/pi)/Highest*100., Highest))
        print("# Dimensionless Period T*sqrt(g/L): {:7.2f}".format(z[3]/math.sqrt(2*pi)))
    #
    print("# Current criterion: {:},  Dimensionless value: {:1.3f}"
          .format(current_name[c_type], current))    
#
def output(n, z, Y, B, Tanh, is_finite):
    """ """
    pi = math.pi
    kd = z[1]
    L=2*pi/z[1]
    H=z[2]/z[1]
    T=z[3]/math.sqrt(z[1])
    c=z[4]/math.sqrt(z[1])
    ce=z[5]/math.sqrt(z[1])
    cs=z[6]/math.sqrt(z[1])
    ubar=z[7]/math.sqrt(z[1])
    Q=ubar-z[8]/pow(kd,1.5)
    R=1+z[9]/z[1]
    
    if is_finite:
        pulse=z[8]+z[1]*z[5];
        ke=0.5*(z[4]*pulse-z[5]*Q*pow(kd,1.5))
    
        # Calculate potential energy, not by computing the mean of 1/2 (eta-d)^2
        # but by exploiting orthogonality of the cosine functions to give the sum of 1/4 Y[i]^2
        pe = 0
        for i in range(1,n):
            pe += 0.25*pow(Y[i], 2)
    
        ub2=2.*z[9]-z[4]*z[4]
        
        sxx=4.*ke-3.*pe+ub2*z[1]+2.*z[5]*(z[7]*z[1]-z[8])
        f=z[4]*(3.*ke-2.*pe)+0.5*ub2*(pulse+z[4]*z[1])+z[4]*z[5]*(z[7]*z[1]-z[8])
        q=z[7]*z[1]-z[8]
        r=z[9]+z[1]
        s=sxx-2.*z[4]*pulse+(z[4]*z[4]+0.5*z[1])*z[1]
    #
    print("# ---------------------------------------------------------")
    print("# Integral quantities - notation from Fenton (1988)")
    if is_finite:
        print("# (1) Quantity, (2) symbol, solution non-dimensionalised by")
        print("# (3) g & wavenumber, and (4) g & mean depth")
        print("# Water depth                        (d) {: 7.4f}  {: 7.4f}".format(z[1], 1.))
    else:
        print("# (1) Quantity, (2) symbol, solution non-dimensionalised by")
        print("# (3) g & wavenumber")
    #
    print("# Wave length                   (lambda) {: 7.4f}  {: 7.4f}".format(2*pi, L))
    print("# Wave height                        (H) {: 7.4f}  {: 7.4f}".format(z[2], H))
    print("# Wave period                      (tau) {: 7.4f}  {: 7.4f}".format(z[3], T))
    print("# Wave speed                         (c) {: 7.4f}  {: 7.4f}".format(z[4], c))
    print("# Eulerian current                 (u1_) {: 7.4f}  {: 7.4f}".format(z[5], ce))
    print("# Stokes current                   (u2_) {: 7.4f}  {: 7.4f}".format(z[6], cs))
    print("# Mean fluid speed in frame of wave (U_) {: 7.4f}  {: 7.4f}".format(z[7], ubar))
    print("# Volume flux due to waves           (q) {: 7.4f}  {: 7.4f}".format(z[8], z[8]/pow(kd,1.5)))
    print("# Bernoulli constant                 (r) {: 7.4f}  {: 7.4f}".format(z[9], z[9]/kd))
    #
    if is_finite:
        print("# Volume flux                        (Q) {: 7.4f}  {: 7.4f}".format(Q*pow(kd,1.5), Q))
        print("# Bernoulli constant                 (R) {: 7.4f}  {: 7.4f}".format(R*kd, R))
        print("# Momentum flux                      (S) {: 7.4f}  {: 7.4f}".format(s, s/kd/kd ))
        print("# Impulse                            (I) {: 7.4f}  {: 7.4f}".format(pulse, pulse/pow(kd,1.5)))
        print("# Kinetic energy                     (T) {: 7.4f}  {: 7.4f}".format(ke, ke/kd/kd))
        print("# Potential energy                   (V) {: 7.4f}  {: 7.4f}".format(pe, pe/kd/kd))
        print("# Mean square of bed velocity     (ub2_) {: 7.4f}  {: 7.4f}".format(ub2, ub2/kd))
        print("# Radiation stress                 (Sxx) {: 7.4f}  {: 7.4f}".format(sxx, sxx/kd/kd))
        print("# Wave power                         (F) {: 7.4f}  {: 7.4f}".format(f, f/pow(kd,2.5)))
    #
    print("# ---------------------------------------------------------")
    print("# Dimensionless coefficients in Fourier series" )
    print("# Potential/Streamfn  Surface elevations" )
    print("#    j          B[j]          E[j]  j=1..n" )
    #print("# N, number of dimensionless Fourier coefficients - j, B[j], & E[j] below", n)
    for i in range (1, n+1):
        print("{:6.0f} {: 1.6e} {: 1.6e}".format(i, B[i], Y[i]))
        #print("%2d\t%15.7e\t%15.7e\n", i, B[i], Y[i]);
    print("" )
    #    
#
#
def print_velo(H, n, z, Y, B, Tanh, surface_points, method):
    """
    Surface velocity
    """
    pi = math.pi
    L = 2 * pi / z[ 1 ]
    # Highest wave - eqn (32) of Fenton (1990)
    Highest = ((0.0077829 * L * L * L + 0.0095721 * L * L + 0.141063 * L)
               / (0.0093407 * L * L * L + 0.0317567 * L * L + 0.078834 * L + 1))
    kd = z[1]
    c = z[4] / math.sqrt(z[1])
    ce = z[5] / math.sqrt(z[1])
    R = 1 + z[9] / z[1]
    X = 0.0
    eta = surface(X, kd, Y, n)
    Velo = [0]
    points = surface_points
    for i in range(0, points):
        y = i * eta / points
        Pressure, Bernoulli_check, u, v, dphidt, ut, vt, ux, uy = point(X, y, kd, Tanh, B, n, ce, c, R)
        # Velo[i] = u
        Velo.append(u)
    #
    i = 1
    sum1 = 0
    while i <= points - 1:
        sum1 += Velo[i]
        i += 2
    # Loop
    i = 2
    sum2 = 0
    while i <= points - 2:
        sum2 += Velo[i]
        i += 2
    # Loop
    ucm = (Velo[0] + 4 * sum1 + 2 * sum2 + Velo[points]) / 3.0 / points
    #
    print("# ---------------------------------------------------------")
    print("{:} {:4.0f} {:1.4e} {:1.4e} {:1.4e} {:1.4e} {:1.4e}"
          .format(method, n, H, L, 0.5 * z[2] / pow(z[1], 3),
                  z[2] / z[1] / Highest * 100.0, ucm))
    #
#
#
def get_Height(MaxH, case, is_finite, L=None, T=None):
    """ """
    case = case.lower()
    if is_finite:
        if case == "wavelength":
            Height = MaxH/L
        else:
            Height = MaxH/(T*T)
    else:
        Height = -MaxH
    
    return Height
#
#
#

