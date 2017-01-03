# Code from Chapter 2 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# This is the start of a script for you to complete
from pylab import *
from numpy import *
import linreg

auto = loadtxt('/Users/srmarsla/Book/Datasets/auto-mpg/auto-mpg.data.txt',comments='"')

# Separate the data into training and testing sets

# Normalise the data

# This is the training part
beta = linreg.linreg(trainin,traintgt)
testin = concatenate((testin,-ones((shape(testin)[0],1))),axis=1)
testout = dot(testin,beta)
error = sum((testout - testtgt)**2)
print error