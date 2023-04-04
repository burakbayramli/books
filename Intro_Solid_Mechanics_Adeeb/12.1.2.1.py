import sympy as sp
import numpy as np
from sympy import lambdify
from matplotlib import pyplot as plt
x = sp.symbols("x")
xL = np.arange(0,1,0.01)
c,L,EA = 1,1,1
# Exact Calculations
u_exact = c*L**2/2/EA*x - c/sp.Rational("6")/EA*x**3
s_exact = u_exact.diff(x)
print("u_exact(x) =",u_exact,"s_exact(x) =",s_exact)
F = lambdify(x,u_exact)
dF = lambdify(x,s_exact)
y_exact = F(xL)
sy_exact = dF(xL)
def PN1(x):
    conds = [(x>=0)&(x<=L/4),(x>=L/4)&(x<L/2)]
    functions = [lambda x:4*x/L,lambda x:4/L*(L/2-x)]
    Dfunctions = [lambda x:4/L,lambda x:-4]
    return np.piecewise(x,conds,functions), np.piecewise(x,conds,Dfunctions)
def PN2(x):
    conds = [(x>=L/4)&(x<=L/2),(x>=L/2)&(x<3*L/4)]
    functions = [lambda x:4/L*(-L/4+x),lambda x:4/L*(3*L/4-x)]
    Dfunctions = [lambda x:4/L,lambda x:-4]
    return np.piecewise(x,conds,functions), np.piecewise(x,conds,Dfunctions)
def PN3(x):
    conds = [(x>=L/2)&(x<=3*L/4),(x>=3*L/4)&(x<=L)]
    functions = [lambda x:4/L*(-L/2+x),lambda x:4/L*(L-x)]
    Dfunctions = [lambda x:4/L,lambda x:-4]
    return np.piecewise(x,conds,functions), np.piecewise(x,conds,Dfunctions)
def PN4(x):
    conds = [(x>=3*L/4)&(x<=L)]
    functions = [lambda x:4/L*(-3*L/4+x)]
    Dfunctions = [lambda x:4/L]
    return np.piecewise(x,conds,functions), np.piecewise(x,conds,Dfunctions)
N1, DN1 = PN1(xL)
N2, DN2 = PN2(xL)
N3, DN3 = PN3(xL)
N4, DN4 = PN4(xL)
u1 = c*L**3/EA*(47/384)
u2 = c*L**3/EA*(11/48)
u3 = c*L**3/EA*(39/128)
u4 = c*L**3/EA*(1/3)
u = u1*N1+u2*N2+u3*N3+u4*N4
up = u1*DN1+u2*DN2+u3*DN3+u4*DN4
fig, ax = plt.subplots(2, figsize=(6,8))
ax[0].set_title("")
ax[0].set_xlabel("x")
ax[0].set_ylabel("displacement")
ax[0].plot(xL,y_exact,label="(c*L^2*x)/(2*EA) - (c*x^3)/(6*EA)")
ax[0].plot(xL,u,label="u_FEA")
ax[1].plot(xL,sy_exact,label="sigma_exact")
ax[1].plot(xL,up,label="sigma_FEA")
ax[1].set_ylabel("sigma11")
for i in ax:
    i.grid(True, which='both')
    i.axhline(y = 0, color = 'k')
    i.axvline(x = 0, color = 'k')
    i.set_xlabel("x")
    i.legend()
plt.savefig('/tmp/out-12.1.2.1.jpg')
