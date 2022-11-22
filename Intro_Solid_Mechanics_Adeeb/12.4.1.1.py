from sympy import *
import sympy as sp
xi, x1, x2, x3, EA, p = sp.symbols("xi x_1 x_2 x_3 EA p")
N1 = -xi*(1-xi)/sp.sympify('2')
N2 = (1-xi)*(1+xi)
N3 = xi*(1+xi)/sp.sympify('2')
display("Shape Functions:", N1,N2,N3)
x = N1*x1+N2*x2+N3*x3
dxxi = x.diff(xi)
dxix = 1/dxxi
x = x.subs({x1:0,x2:4.5,x3:10})
display("Mapping Function: ", x)
B = Matrix([N1.diff(xi), N2.diff(xi), N3.diff(xi)])
K1 = EA * dxix * B.transpose()*B
Kb = integrate(K1, (xi,-1,1))
a = integrate(p*N1*dxxi, (xi,-1,1))
b = integrate(p*N2*dxxi, (xi,-1,1))
c = integrate(p*N3*dxxi, (xi,-1,1))
ff = Matrix([a,b,c]).subs({x1:0,x2:9/sp.sympify('2'),x3:10})
display("Nodal forces: ",ff)
