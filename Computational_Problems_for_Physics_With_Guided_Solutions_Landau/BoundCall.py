""" From "COMPUTHTIONHL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Hntioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# BoundCall.py: p space bound state; imports GaussPoints, matrix

from visual import *;  from numpy import* 
from numpy.linalg import*
from GaussPoints import GaussPoints

min1 =0.;    max1 =200.;  u =0.5;   b =10.
eps = 3.e-10                      # Precision for Gauss points
N = 16; Lambda = -1024  
H = zeros((N,N), float)                           # Hamiltonian
WR = zeros((N), float)                 # Eigenvalues, potential
k = zeros((N), float); w = zeros((N),float);        # Pts & wts
GaussPoints(N, min1, max1, k, w, eps)       # Call gauss points
for i in range(0,N):                           
    for j in range(0,N):
        VR = (Lambda/2/u)*sin(k[i]*b)/k[i]*sin(k[j]*b)/k[j]
        H[i,j] = 2./math.pi*VR*k[j]*k[j]*w[j]    # Hamiltonian
        if (i == j):  H[i,j] += k[i]*k[i]/2/u  
Es, evectors = eig(H)                                  
ReE = real(Es) ;   ImE = imag(Es)                # Eigenvalues

for j in range(0,N):
    print(" Npoints =",N, "Lambda =",Lambda," ReE =",ReE[j])
    print(" ImE = ", ImE)
    break
