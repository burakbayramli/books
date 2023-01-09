from sympy import *
from numpy import *
import sympy as sp 
import numpy as np
import matplotlib.pyplot as plt
sp.init_printing(use_latex = "mathjax")
x, a2, a3, a4, a5, a6, a7, EI, L, E= symbols("x a_2 a_3 a_4 a_5 a_6 a_7 EI L E")
print("Exact")
b = 1/4
L = 8
t = (1/2-1/4*x/L)
I = b*t**3/12
E = 20*10**9
P = 10000
# Exact solution done in mathematica code
y = (34.8872 + (-2.96688 + 0.036864*x)*x + (-12.5829 + 0.786432*x)*sp.ln(16-x))/(x-16)
M = -80000 + 10000*x
V = M.diff(x)
print("y,M,V: ", y, M, V)
print("Second Order Polynomial")
y2 = a2*x**2
d2y = y2.diff(x,2)
PE2 = integrate(E*I/2*d2y**2, (x,0,L)) - (-P)*y2.subs(x,L)
print("Potential Energy: ", PE2)
sol = solve(PE2.diff(a2), a2)
y2 = y2.subs(a2,sol[0])
M2 = E*I*y2.diff(x,2)
V2 = M2.diff(x)
print("y2,M2,V2: ", y2, M2, V2)
print("Third Order Polynomial")
y3 = a2*x**2+a3*x**3
d2y = y3.diff(x,2)
PE3 = integrate(E*I/2*d2y**2, (x,0,L)) - (-P)*y3.subs(x,L)
print("Potential Energy: ", PE3)
sol = solve((PE3.diff(a2), PE3.diff(a3)), (a2,a3))
y3 = y3.subs({a2:sol[a2], a3:sol[a3]})
M3 = E*I*y3.diff(x,2)
V3 = M3.diff(x)
print("y3,M3,V3: ", y3, M3, V3)
print("Fourth Order Polynomial")
y4 = a2*x**2+a3*x**3+a4*x**4
d2y = y4.diff(x,2)
PE4 = integrate(E*I/2*d2y**2, (x,0,L)) - (-P)*y4.subs(x,L)
print("Potential Energy: ", PE4)
sol = solve((PE4.diff(a2), PE4.diff(a3), PE4.diff(a4)), (a2,a3,a4))
y4 = y4.subs({a2:sol[a2], a3:sol[a3], a4:sol[a4]})
M4 = E*I*y4.diff(x,2)
V4 = M4.diff(x)
print("y4,M4,V4: ", y4, M4, V4)
print("Fifth Order Polynomial")
y5 = a2*x**2+a3*x**3+a4*x**4+a5*x**5
d2y = y5.diff(x,2)
PE5 = integrate(E*I/2*d2y**2, (x,0,L)) - (-P)*y5.subs(x,L)
print("Potential Energy: ", PE5)
sol = solve((PE5.diff(a2), PE5.diff(a3), PE5.diff(a4), PE5.diff(a5)), (a2,a3,a4,a5))
y5 = y5.subs({a2:sol[a2], a3:sol[a3], a4:sol[a4], a5:sol[a5]})
M5 = E*I*y5.diff(x,2)
V5 = M5.diff(x)
print("y5,M5,V5: ", y5, M5, V5)
print("Sixth Order Polynomial")
y6 = a2*x**2+a3*x**3+a4*x**4+a5*x**5+a6*x**6
d2y = y6.diff(x,2)
PE6 = integrate(E*I/2*d2y**2, (x,0,L)) - (-P)*y6.subs(x,L)
print("Potential Energy: ", PE6)
sol = solve((PE6.diff(a2), PE6.diff(a3), PE6.diff(a4), PE6.diff(a5), PE6.diff(a6)), (a2,a3,a4,a5,a6))
y6 = y6.subs({a2:sol[a2], a3:sol[a3], a4:sol[a4], a5:sol[a5], a6:sol[a6]})
M6 = E*I*y6.diff(x,2)
V6 = M6.diff(x)
print("y6,M6,V6: ", y6, M6, V6)
print("Seventh Order Polynomial")
y7 = a2*x**2+a3*x**3+a4*x**4+a5*x**5+a6*x**6+a7*x**7
d2y = y7.diff(x,2)
PE7 = integrate(E*I/2*d2y**2, (x,0,L)) - (-P)*y7.subs(x,L)
print("Potential Energy: ", PE7)
sol = solve((PE7.diff(a2), PE7.diff(a3), PE7.diff(a4), PE7.diff(a5), PE7.diff(a6), PE7.diff(a7)), (a2,a3,a4,a5,a6,a7))
y7 = y7.subs({a2:sol[a2], a3:sol[a3], a4:sol[a4], a5:sol[a5], a6:sol[a6], a7:sol[a7]})
M7 = E*I*y7.diff(x,2)
V7 = M7.diff(x)
print("y7,M7,V7: ", y7, M7, V7)
#Graph
fig, ax = plt.subplots(3,1, figsize = (10,15))
plt.setp(ax[0], xlabel = "x(m) ", ylabel = "y(m)")
plt.setp(ax[1], xlabel = "x(m)", ylabel = "M(N*m)")
plt.setp(ax[2], xlabel = "x(m)", ylabel = "V(N)")
x1 = np.arange(0,8.1,0.1)
y_list = [y.subs({x:i}) for i in x1]
y2_list = [y2.subs({x:i}) for i in x1]
y3_list = [y3.subs({x:i}) for i in x1]
y4_list = [y4.subs({x:i}) for i in x1]
y5_list = [y5.subs({x:i}) for i in x1]
y6_list = [y6.subs({x:i}) for i in x1]
y7_list = [y7.subs({x:i}) for i in x1]
M_list = [M.subs({x:i}) for i in x1]
M2_list = [M2.subs({x:i}) for i in x1]
M3_list = [M3.subs({x:i}) for i in x1]
M4_list = [M4.subs({x:i}) for i in x1]
M5_list = [M5.subs({x:i}) for i in x1]
M6_list = [M6.subs({x:i}) for i in x1]
M7_list = [M7.subs({x:i}) for i in x1]
V_list = [V.subs({x:i}) for i in x1]
V2_list = [V2.subs({x:i}) for i in x1]
V3_list = [V3.subs({x:i}) for i in x1]
V4_list = [V4.subs({x:i}) for i in x1]
V5_list = [V5.subs({x:i}) for i in x1]
V6_list = [V6.subs({x:i}) for i in x1]
V7_list = [V7.subs({x:i}) for i in x1]
ax[0].plot(x1, y_list, 'blue', label = "Exact y")
ax[0].plot(x1, y2_list, 'orange', label = "y2")
ax[0].plot(x1, y3_list, 'green', label = "y3")
ax[0].plot(x1, y4_list, 'red', label = "y4")
ax[0].plot(x1, y5_list, 'purple', label = "y5")
ax[0].plot(x1, y6_list, 'brown', label = "y6")
ax[0].plot(x1, y7_list, 'cyan', label = "y7")
ax[0].legend()
ax[1].plot(x1, M_list, 'blue', label = "Exact M")
ax[1].plot(x1, M2_list, 'orange', label = "y2")
ax[1].plot(x1, M3_list, 'green', label = "y3")
ax[1].plot(x1, M4_list, 'red', label = "y4")
ax[1].plot(x1, M5_list, 'purple', label = "y5")
ax[1].plot(x1, M6_list, 'brown', label = "y6")
ax[1].plot(x1, M7_list, 'cyan', label = "y7")
ax[1].legend()
ax[2].plot(x1, V_list, 'blue', label = "Exact V")
ax[2].plot(x1, V2_list, 'orange', label = "y2")
ax[2].plot(x1, V3_list, 'green', label = "y3")
ax[2].plot(x1, V4_list, 'red', label = "y4")
ax[2].plot(x1, V5_list, 'purple', label = "y5")
ax[2].plot(x1, V6_list, 'brown', label = "y6")
ax[2].plot(x1, V7_list, 'cyan', label = "y7")
ax[2].legend()
