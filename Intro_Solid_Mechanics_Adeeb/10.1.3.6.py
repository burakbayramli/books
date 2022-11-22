from sympy import Function,dsolve,diff,integrate,Poly,solve,simplify
import numpy as np
import matplotlib.pyplot as plt
y = Function("y")
x,EI,L,P = sp.symbols("x EI L P")
a2,a3,a2s,a3s = sp.symbols("a_2 a_3 a_2s a_3s")
y1 = y(x).subs({x:0})
th2 = y(x).diff(x).subs(x,L/2)
M1 = y(x).diff(x,2).subs(x,0)
V2 = y(x).diff(x,3).subs(x,L/2)
s = dsolve(y(x).diff(x,4),y(x),ics={y1:0,th2:0,M1:0,V2:P/2/EI})
y1 = s.rhs
y2 = simplify(y1.subs({x:L-x}))
yapprox = a2*x*(x-L)+a3*x*(x**2-L**2)
ystar = yapprox.subs({a2:a2s,a3:a3s})
EVW = (-P*ystar).subs({x:L/2}).expand()
IVW = integrate(EI*diff(yapprox,x,x)*diff(ystar,x,x),(x,0,L)).expand()
Eq1 = IVW.coeff(a2s)-EVW.coeff(a2s)
Eq2 = IVW.coeff(a3s)-EVW.coeff(a3s)
s = solve({Eq1,Eq2},{a2,a3})
yapprox = yapprox.subs({a2:s[a2],a3:s[a3]})
y1exactp = y1.subs({L:1,EI:1,P:1})
y2exactp = y2.subs({L:1,EI:1,P:1})
yp = yapprox.subs({L:1,EI:1,P:1})
xL = xrange = np.arange(0,1.01,0.01)
Y1 = []
Y2 = []
for i in range(len(xL)):
    Y1.append(yp.subs({x:xL[i]}))
    #piecewise for exact : y1 when 0<=x<L/2, y2 when L/2=<x<L
    if i<(len(xL)/2):
        Y2.append(y1exactp.subs({x:xL[i]}))
    else:
        Y2.append(y2exactp.subs({x:xL[i]}))
fig, ax = plt.subplots()
plt.xlabel("x(length units)")
plt.ylabel("y(length units)")
ax.plot(xL, Y1, label="Approx")
ax.plot(xL, Y2, label="Exact")
ax.legend()
ax.grid(True, which='both')
ax.axhline(y = 0, color = 'k')
ax.axvline(x = 0, color = 'k')
