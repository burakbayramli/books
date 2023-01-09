from sympy import *
from numpy import *
import sympy as sp
import numpy as np
import matplotlib.pyplot as plt
sp.init_printing(use_latex = "mathjax")
a, L, x , EI, psi, kAG, C1, C2, C3, C4 = symbols("a L x EI psi kAG C_1 C_2 C_3 C_4")
q = -a
fig, ax = plt.subplots(2,2, figsize = (10,6))
plt.setp(ax[0,0], xlabel = "x", ylabel = "y")
plt.setp(ax[0,1], xlabel = "x", ylabel = "Psi")
plt.setp(ax[1,0], xlabel = "x", ylabel = "M")
plt.setp(ax[1,1], xlabel = "x", ylabel = "V")

# With general integration
EIpsi = integrate(q,x,x,x) + C1*x**2/2 + C2*x + C3 
print(EIpsi)
EIy = integrate(q,x,x,x,x) + C1*x**3/6 + C2*x**2/2 + C3*x + C4 - EI/kAG*(integrate(q,x,x) + C1*x) 
print(EIy)
M = EIpsi.diff(x)
V = EIpsi.diff(x,2)
print(M,V)
M1 = M.subs(x,0)
M2 = M.subs(x,L)
y1 = (EIy/EI).subs(x,0)
y2 = (EIy/EI).subs(x,L)
print(M1,M2,y1,y2)
s = solve([M1,M2,y1,y2], [C1, C2, C3, C4])
print(s)
x1 = np.arange(0,1.1,0.1)
EIpsi = EIpsi.subs({C1:s[C1], C2:s[C2], C3:s[C3], C4:s[C4], a:1, L:1, kAG:1, EI:1})
psi_list = [EIpsi.subs({x:i}) for i in x1]
print(EIpsi)
EIy = EIy.subs({C1:s[C1], C2:s[C2], C3:s[C3], C4:s[C4], a:1, L:1, kAG:1, EI:1})
y_list = [EIy.subs({x:i}) for i in x1]
print(EIy)
M = M.subs({C1:s[C1], C2:s[C2], C3:s[C3], a:1, L:1, kAG:1, EI:1})
M_list = [M.subs({x:i}) for i in x1]
print(M)
V = V.subs({C1:s[C1], C2:s[C2], C3:s[C3], a:1, L:1, kAG:1, EI:1})
V_list = [V.subs({x:i}) for i in x1]
print(V)
ax[0,0].plot(x1, y_list)
ax[0,1].plot(x1, psi_list)
ax[1,0].plot(x1, M_list)
ax[1,1].plot(x1, V_list)
