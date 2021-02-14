""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# Bisection.py: zero of f(x) via Bisection algorithm within [a,b]

from visual.graph import *
eps = 1e-3;  Nmax = 100;  a = 0.0; b = 7.0     # Precision, [a,b]
                          
def f(x): return 2*math.cos(x) - x           # Your function here
    
def Bisection(Xminus, Xplus, Nmax, eps):          # Do not change    
   for it in range(0, Nmax):
       x = (Xplus +  Xminus)/2.                      
       print(" it =", it, " x = ", x, " f(x) =", f(x))
       if (f(Xplus)*f(x) > 0.): Xplus = x       # Change x+ to x
       else: Xminus =  x                        # Change x- to x
       if (abs(f(x) ) < eps):                   # Converged?
          print("\n Root found with precision eps = ", eps)
          break
       if it == Nmax-1: print ("\n No root after N iterations\n")
   return x

root = Bisection(a, b, Nmax, eps)
print(" Root =", root)
