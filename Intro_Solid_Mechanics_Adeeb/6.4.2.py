from sympy import diff,dsolve,simplify
import sympy as sp
x, rho, g, L, P = sp.symbols("x rho g L P")
f = sp.Function('f')
A = sp.pi*(2-x/L)**2
# sub in L into A
Al = A.subs({x:L})
print("area varying with X_2")
print("A =",A)
print("A x@L=",Al)
# partial derivative with respect to x, select f(L)=P/A 
sol = dsolve(f(x).diff(x)*A+A.diff(x)*f(x)+rho*g*A,f(x),ics={f(L):P/Al})
fsol=sol.rhs
print('solution f(x)=',fsol)
print("comparison with the Mathematica solution")
onethird = sp.Rational('1/3')
s = onethird*(2*rho*g*L-rho*g*x+L**2*(3*P-rho*g*L*sp.pi)/sp.pi/(x-2*L)**2)
print("Mathematica solution:",s)
dif = simplify(s-fsol)
print("difference:",dif)
