import sympy as sp
from sympy import Function,dsolve,diff,simplify,integrate
x,EI,L = sp.symbols("x EI L")
y = Function("y")
a = sp.symbols("a")
y1 = y(x).subs(x,0)
y2 = y(x).subs(x,L)
yp1 = y(x).diff(x).subs(x,0)
yp2 = y(x).diff(x).subs(x,L)
q = -5*x
s = dsolve(EI*y(x).diff(x,4)-q,y(x),ics={y1:0,y2:0,yp1:0,yp2:0})
y = s.rhs
print("y(x) =",y)
th = simplify(diff(y,x))
M = simplify(EI*diff(y,x,x))
V = simplify(EI*diff(y,x,x,x))
print("th =",th,"M =",M,"V =",V)
M1 = M.subs({x:0})
M2 = M.subs({x:L})
V1 = V.subs({x:0})
V2 = V.subs({x:L})
print("M@x=0",M1,"M@x=L",M2,"V@x=0",V1,"V@x=L",V2)
ystar = a*x**2
thstar = diff(ystar,x)
ystar1 = ystar.subs({x:0})
ystar2 = ystar.subs({x:L})
thstar1 = thstar.subs({x:0})
thstar2 = thstar.subs({x:L})
print("y*@x=0",ystar1,"y*@x=L",ystar2,"\u03B8*@x=0",thstar1,"\u03B8*@x=L",thstar2)
IVW = integrate(M*diff(ystar,x,x),(x,0,L))
print("Internal Virtual Work =",IVW)
EVWq = integrate(q*ystar,(x,0,L))
print("EVWq =",EVWq)
EVW1 = V1*ystar1-M1*thstar1
EVW2 = M2*thstar2-V2*ystar2
print("EVW1 =",EVW1,"EVW2 =",EVW2)
EVW = EVWq+EVW1+EVW2
print("External Virtual Work = Internal Virtual Work =",EVW)
