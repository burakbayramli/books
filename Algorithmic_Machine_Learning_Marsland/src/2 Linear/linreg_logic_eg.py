
# Code from Chapter 2 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# Demonstration of the Perceptron and Linear Regressor on the basic logic functions

from numpy import *
import linreg

inputs = array([[0,0],[0,1],[1,0],[1,1]])
testin = concatenate((inputs,-ones((shape(inputs)[0],1))),axis=1)

# AND data
ANDtargets = array([[0],[0],[0],[1]])
# OR data
ORtargets = array([[0],[1],[1],[1]])
# XOR data
XORtargets = array([[0],[1],[1],[0]])

print "AND data"
ANDbeta = linreg.linreg(inputs,ANDtargets)
ANDout = dot(testin,ANDbeta)
print ANDout

print "OR data"
ORbeta = linreg.linreg(inputs,ORtargets)
ORout = dot(testin,ORbeta)
print ORout

print "XOR data"
XORbeta = linreg.linreg(inputs,XORtargets)
XORout = dot(testin,XORbeta)
print XORout