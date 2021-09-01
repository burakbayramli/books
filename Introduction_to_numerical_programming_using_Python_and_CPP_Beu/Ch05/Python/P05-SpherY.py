# Checks the addition theorem for spherical harmonics
from math import *
from specfunc import *

l = 5
theta1 = pi/5; phi1 = pi/9
theta2 = pi/3; phi2 = pi/8

cosgam = cos(theta1) * cos(theta2) \
       + sin(theta1) * sin(theta2) * cos(phi2 - phi1)
(P,d) = Legendre(l,cosgam)

sumRe = 0e0
sumIm = 0e0
for m in range(-l,l+1):
    (ReY1,ImY1) = SpherY(l, m, theta1, phi1)
    (ReY2,ImY2) = SpherY(l, m, theta2, phi2)

    sumRe += ReY2 * ReY1 + ImY2 * ImY1
    sumIm += ReY2 * ImY1 - ImY2 * ReY1

sumRe *= 4*pi/(2*l+1)
sumIm *= 4*pi/(2*l+1)

print(P-sumRe,sumIm)                        # check: P - sumRe = 0, sumIm = 0
