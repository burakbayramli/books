""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# Entangle.py: Calculate quantum entangled states

from numpy import * ; from numpy.linalg import *

nmax = 4
H = zeros((nmax,nmax),float)
XAXB = array([[0, 0, 0, 1],[0,0,1,0],[0,1,0,0],[1,0,0,0]])
YAYB = array([[0,0,0,-1],[0,0,1,0],[0,1,0,0],[-1,0,0,0]])
ZAZB = array([[1,0,0,0],[0,-1,0,0],[0,0,-1,0],[0,0,0,1]])
SASB = XAXB + YAYB + ZAZB - 3*ZAZB       # Hamiltonian 
print '\nHamiltonian without mu^2/r^3 factor \n', SASB, '\n'
 
es,ev = eig(SASB)            # Eigenvalues and eigenvectors
print 'Eigenvalues\n', es, '\n'
print "Eigenvectors (in columns)\n", ev, "\n"

phi1 = (ev[0,0],ev[1,0],ev[2,0],ev[3,0])  # Eigenvectors
phi4 = (ev[0,1],ev[1,1],ev[2,1],ev[3,1])  
phi3 = (ev[0,2],ev[1,2],ev[2,2],ev[3,2])
phi2 = (ev[0,3],ev[1,3],ev[2,3],ev[3,3])
 
basis=[phi1, phi2, phi3,phi4]        # List eigenvectors

for i in range(0,nmax):         # Hamiltonian in new basis
    for j in range(0,nmax):
       term   = dot(SASB,basis[i])
       H[i,j] = dot(basis[j],term)
print "Hamiltonian in Eigenvector Basis\n", H    