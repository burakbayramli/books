from sympy import *
from numpy import *
import sympy as sp
import numpy as np
import matplotlib.pyplot as plt
sp.init_printing(use_latex = "mathjax")
a, L, x , EI = symbols("a L x EI")
q = -a
fig, ax = plt.subplots(2,2, figsize = (10,6))
plt.setp(ax[0,0], xlabel = "x", ylabel = "y")
plt.setp(ax[0,1], xlabel = "x", ylabel = "M")
plt.setp(ax[1,1], xlabel = "x", ylabel = "th")
plt.setp(ax[1,0], xlabel = "x", ylabel = "V")
y = Function("y")
th = y(x).diff(x)
M = EI * y(x).diff(x,2)
V = EI * y(x).diff(x,3)
M1 = M.subs(x,0)
M2 = M.subs(x,L)
V1 = V.subs(x,0)
V2 = V.subs(x,L)
y1 = y(x).subs(x,0)
y2 = y(x).subs(x,L)
s = dsolve(EI*y(x).diff(x,4)-q, y(x),ics={M1/EI:0,M2/EI:0,y1:0,y2:0})
y = s.rhs
th = y.diff(x)
M = EI * y.diff(x,2)
V = EI * y.diff(x,3)
x1 = np.arange(0,1.1,0.1)
y = y.subs({L:1, a:1, EI:1})
y_list = [y.subs({x:i}) for i in x1]
th = th.subs({L:1, a:1, EI:1})
th_list = [th.subs({x:i}) for i in x1]
M = M.subs({L:1, a:1, EI:1})
M_list = [M.subs({x:i}) for i in x1]
V = V.subs({L:1, a:1, EI:1})
V_list = [V.subs({x:i}) for i in x1]
ax[0,0].plot(x1, y_list)
ax[0,1].plot(x1, M_list)
ax[1,0].plot(x1, V_list)
ax[1,1].plot(x1, th_list)
