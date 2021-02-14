""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# ODEsympy.py: symbolic soltn of HO ODE using sympy

from sympy import *

f, g = symbols('f g',cls=Function)      # makes f a function 
t, kap, w0 = symbols('t kap w0')
f(t)
f(t).diff(t)
 
 # The ODE
diffeq = Eq(f(t).diff(t,t) + kap*(f(t).diff(t)) + (w0*w0)*f(t))
print "\n ODE to be solved:" 
print diffeq
print "\n Solution of ODE:"
ff = dsolve(diffeq,f(t))     # Solves ODE
F = ff.subs(t,0)
print ff