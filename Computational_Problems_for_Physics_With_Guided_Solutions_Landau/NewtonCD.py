""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# NewtonCD.py    Newton Search with central difference

from math import cos

x = 1111.;  dx = 3.e-4; eps = 0.002; Nmax = 100;        # Parameters 

def f(x):  return 2*cos(x) - x # Function

for it in range(0, Nmax + 1):
    F = f(x)
    if (abs(F) <= eps):                              # Converged?  
        print("\n Root found, f(root) =", F, ", eps = " , eps) 
        break
    print("Iteration # = ", it, " x = ", x, " f(x) = ", F)
    df = (f(x+dx/2)  -  f(x-dx/2))/dx               # Central diff
    dx = - F/df 
    x   += dx                                          # New guess
