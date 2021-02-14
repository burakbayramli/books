""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# TrapMethods.py: trapezoid integration, a<x<b, N pts, N-1 intervals 

from numpy import *   

def func(x):
    return 5*(sin(8*x))**2*exp(-x*x)-13*cos(3*x)

def trapezoid(A,B,N):
    h = (B - A)/(N - 1)                     # step size 
    sum = (func(A)+func(B))/2               # (1st + last)/2
    for i in range(1, N-1):     
       sum += func(A+i*h)       
    return h*sum                
A = 0.5
B = 2.3
N = 1200
print(trapezoid(A,B,N-1)) 