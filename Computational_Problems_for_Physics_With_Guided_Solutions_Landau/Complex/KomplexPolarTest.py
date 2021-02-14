""" From "A SURVEY OF COMPUTATIONAL PHYSICS", Python eBook Version
   by RH Landau, MJ Paez, and CC Bordeianu
   Copyright Princeton University Press, Princeton, 2012; Book  Copyright R Landau, 
   Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2012.
   Support by National Science Foundation , Oregon State Univ, Microsoft Corp"""  

# KomplexPolarTest.py: tests Komplex package

from KomplexPolar import Komplex        # bring Komplex from KomplexPolar
from sys import version
if int(version[0])>2:    # raw_input deprecated in Python 3
    raw_input = input   
import math

a = Komplex(1.0, 1.0, 1)                # initialize Komplex a :cartesian
b = Komplex(1.0, 2.0, 1)                # initialize Komplex b :cartesian

print("Cartesian: Re a =  ", a.re, " Im a =  ", a.im)   # real & im parts
print("Cartesian: Re b =  ", b.re, " Im b =  ", b.im)   
c = Komplex.add(a, b, 1)                                      # a + b = c
print("Cartesian:  c =  a + b = (", c.re, ", ", c.im, ")")# re & im parts
e = b
print("Cartesian:  e =  b = (", e.re, ", ", e.im, ")")# Komplex cartesian
"Polar version, uses get and set methods"            # now polar examples
a = Komplex(math.sqrt(2.0), math.pi/4.0, 0)            #  use R and theta
b = Komplex(math.sqrt(5.), math.atan2(2.0, 1.0), 0)   # b = (R, theta, 0)
print("Polar: Re a = ", a.getRe(), ", Im a = ", a.getIm()) # cartesian re
print("Polar: Re b = ", b.getRe(), ", Im b = ", b.getIm()) # cartesian im
c = Komplex.add(a, b, 0)                                     # a + b  = c
e = c                                                         # defines e
print("Polar: Re e = ", e.getRe(), ", Im e = ", e.getIm())  # carte re,im
print("Enter any character to finish")
s = raw_input()
