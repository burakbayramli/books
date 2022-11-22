from sympy import *
from numpy import * 
import sympy as sp 
import numpy as np
import matplotlib.pyplot as plt
sp.init_printing(use_latex = "mathjax")
x, a1, a2, a3, E, P = symbols("x a_1 a_2 a_3 E P")
Ax = 25/100*(5/10-25/200*x)
display("Exact")
u = Function("u")
u1_1 = u(x).subs(x,0)
u2_1 = u(x).diff(x).subs(x,L)
Ax1 = Ax.subs(x,L)
display("Area: ", Ax)
s = dsolve(E*u(x).diff(x,2)*Ax+E*u(x).diff(x)*Ax.diff(x), u(x), ics = {u1_1:0, u2_1:200/E/Ax1})
u = s.rhs.subs({E:100000, L:2, P:200})
stress0 = (E * u.diff(x)).subs({E:100000, L:2, P:200})
display("displacement and stress: ", u, stress0)
display("First Degree")
u1 = a1*x 
uL = u1.subs(x,L)
PE = integrate((1/2)*E*((u1.diff(x))**2)*Ax, (x,0,L)) - P*uL
PE = PE.subs({E:100000, L:2, P:200})
display("Potential Energy: ", PE)
Eq1 = PE.diff(a1)
display("Minimize:", Eq1)
s = solve(Eq1,a1)
display("Solve: ", s)
u1 = u1.subs(a1,s[0])
display("Best First degree Polynomial(Rayleigh Ritz method): ", u1)
stress1 = (E * u1.diff(x)).subs(E,100000)
display("stress: ", stress1)
display("Second Degree")
u2 = a1*x+a2*x**2
PE2 = integrate((1/2)*E*((u2.diff(x))**2)*Ax, (x,0,L)) - P*u2.subs(x,L)
PE2 = PE2.subs({E:100000, L:2, P:200})
display("Potential Energy: ", PE2)
Eq1_2 = PE2.diff(a1)
Eq2_2 = PE2.diff(a2)
s = solve((Eq1_2, Eq2_2),a1, a2)
display("Minimize:", Eq1_2, Eq2_2)
display("Solve: ", s)
u2 = u2.subs({a1:s[a1], a2:s[a2]})
display("Best Second degree Polynomial(Rayleigh Ritz method): ", u2)
stress2 = (E * u2.diff(x)).subs(E,100000)
display("stress: ", stress2)
display("Third Degree")
u3 = a1*x+a2*x**2+a3*x**3
PE3 = integrate((1/2)*E*((u3.diff(x))**2)*Ax, (x,0,L)) - P*u3.subs(x,L)
PE3 = PE3.subs({E:100000, L:2, P:200})
display("Potential Energy: ", PE3)
Eq1_3 = PE3.diff(a1)
Eq2_3 = PE3.diff(a2)
Eq3_3 = PE3.diff(a3)
s = solve((Eq1_3, Eq2_3, Eq3_3),a1, a2, a3)
display("Minimize:", Eq1_3, Eq2_3, Eq3_3)
display("Solve: ", s)
u3 = u3.subs({a1:s[a1], a2:s[a2], a3:s[a3]})
display("Best Third degree Polynomial(Rayleigh Ritz method): ", u3)
stress3 = (E * u3.diff(x)).subs(E,100000)
display("stress: ", stress3)
fig, ax = plt.subplots(1,2, figsize = (15,6))
plt.setp(ax[0], xlabel = "X1 m ", ylabel = "u(m)")
plt.setp(ax[1], xlabel = "X1 m", ylabel = "Stress(Pa)")
x1 = np.arange(0,2.5,0.5)
u_list = [u.subs({x:i}) for i in x1]
u1_list = [u1.subs({x:i}) for i in x1]
u2_list = [u2.subs({x:i}) for i in x1]
u3_list = [u3.subs({x:i}) for i in x1]
stress0_list = [stress0.subs({x:i}) for i in x1]
stress1_list = [stress1.subs({x:i}) for i in x1]
stress2_list = [stress2.subs({x:i}) for i in x1]
stress3_list = [stress3.subs({x:i}) for i in x1]
display("Comparison")
ax[0].plot(x1, u_list, 'blue', label = "exact")
ax[0].plot(x1, u1_list, 'orange', label = "u1")
ax[0].plot(x1, u2_list, 'green', label = "u2")
ax[0].plot(x1, u3_list, 'red', label = "u3")
ax[0].legend()
ax[1].plot(x1, stress0_list, 'blue', label = "exact")
ax[1].plot(x1, stress1_list, 'orange', label = "u1")
ax[1].plot(x1, stress2_list, 'green', label = "u2")
ax[1].plot(x1, stress3_list, 'red', label = "u3")
ax[1].legend()
