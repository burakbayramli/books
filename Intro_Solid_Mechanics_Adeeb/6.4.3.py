from sympy import Eq,diff,integrate, dsolve
import sympy as sp
sp.init_printing(use_latex="mathjax")
rho, b1, x1, c, s = sp.symbols("rho b_1 x_1 C sigma")
f = sp.Function("f")
equation = f(x1).diff(x1)+rho*b1
print("equation of equilibrium =",Eq(equation,0))
#intEqu = integrate(equation, x1)
#intEqu = intEqu.subs({b1:10,rho:1})
sol = dsolve(equation.subs({b1:10,rho:1}),f(x1),ics={f(5):20})
print("solution for the stress f(x1) (N/m^2)",sol)
