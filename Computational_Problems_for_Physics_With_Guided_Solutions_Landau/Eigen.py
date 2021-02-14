""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# Eigen.py  Solution of matrix eigenvalue problem

from numpy import*
from numpy.linalg import eig

I = array( [[2./3,-1./4,-1./4], [-1./4,2./3,-1./4], [-1./4,-1./4,2./3]] )
print('\n I =\n', I)

Es, evectors = eig(I)                            # Solves eigenvalue problem
print('\n Eigenvalues = \n', Es)
print('\n Matrix of Eigenvectors =\n', evectors)

Vec = array([ evectors[0, 0], evectors[0, 1], evectors[0, 2] ] )
print('\n A single eigenvector to test RHS vs LHS =', Vec, '\n')

LHS = dot(I, Vec)
RHS = dot(Vec, Es[0])
print('LHS - RHS =\n', LHS-RHS) 