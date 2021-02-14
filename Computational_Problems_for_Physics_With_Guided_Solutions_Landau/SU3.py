""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# SU3.py: SU3 matrix manipulations

from numpy import *
from numpy.linalg import *

L1 = array([[0,1,0],[1,0,0],[0,0,0]])        # Eight generators
L2 = array([[0,-1j,0],[1j,0,0],[0,0,0]])
L3 = array([[1,0,0],[0,-1,0],[0,0,0]])
L4 = array([[0,0,1],[0,0,0],[1,0,0]])
L5 = array([[0,0,-1j],[0,0,0],[1j,0,0]])
L6 = array([[0,0,0],[0,0,1],[0,1,0]])
L7 = array([[0,0,0],[0,0,-1j],[0,1j,0]])
L8 = array([[1,0,0],[0,1,0],[0,0,-2]])*1/sqrt(3)
u = array([1,0,0])                                   # Up quark
d = array([0,1,0])                                 # Down quark
s = array([0,0,1])                              # Strange quark
Ip = 0.5*(L1+1j*L2)                         # Raising operators
Up = 0.5*(L6+1j*L7)
Vp = 0.5*(L4+1j*L5)
Im = 0.5*(L1-1j*L2)                        # Lowering operators
Um = 0.5*(L6-1j*L7)
Vm = 0.5*(L4-1j*L5)
Ipxd = dot(Ip,d)                                 # Raise d to u
print("Ipxd", Ipxd)
Vpxs = dot(Vp, s)                                # Raise s to u
print("Vpxs", Vpxs)
Upxs=dot(Up,s)                                   # Raise s to d
print("Upxs ", Upxs) 

