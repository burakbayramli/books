import sympy
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

t = sympy.symbols("t", positive=True)
y = sympy.Function("y")
#gamma = sympy.Function("gamma")

t, omega0, gamma= sympy.symbols("t, omega_0, gamma", positive=True)
#t, omega0= sympy.symbols("t, omega_0", positive=True)
x = sympy.Function("x")
ode = x(t).diff(t, 2) + 2 * gamma * omega0 * x(t).diff(t) + omega0**2 * x(t)
#ode = x(t).diff(t, 2) + 2 * gamma(t) * omega0 * x(t).diff(t) + omega0**2 * x(t)
ode_sol = sympy.dsolve(ode)
print (sympy.pprint(ode_sol))

ics = {x(0): 1, x(t).diff(t).subs(t, 0): 0}

def apply_ics(sol, ics, x, known_params):
    free_params = sol.free_symbols - set(known_params)
    eqs = [(sol.lhs.diff(x, n) - sol.rhs.diff(x, n)).subs(x, 0).subs(ics)
           for n in range(len(ics))]
    sol_params = sympy.solve(eqs, free_params)
    return sol.subs(sol_params)


x_t_sol = apply_ics(ode_sol, ics, t, [omega0, gamma])

#print (sympy.pprint(x_t_sol))

t = sympy.symbols("t", positive=True)

s, Y = sympy.symbols("s, Y", real=True)

y = sympy.Function("y")

ode = y(t).diff(t, 2) + 2 * y(t).diff(t) + 10 * y(t) - 2 * sympy.sin(3*t)

L_y = sympy.laplace_transform(y(t), t, s)

L_ode = sympy.laplace_transform(ode, t, s, noconds=True)

def laplace_transform_derivatives(e):
    """
    Evaluate the laplace transforms of derivatives of functions
    """
    if isinstance(e, sympy.LaplaceTransform):
        if isinstance(e.args[0], sympy.Derivative):
            d, t, s = e.args
            n = d.args[1][1]
            return ((s**n) * sympy.LaplaceTransform(d.args[0], t, s) - 
                    sum([s**(n-i) * sympy.diff(d.args[0], t, i-1).subs(t, 0)
                         for i in range(1, n+1)]))
        
    if isinstance(e, (sympy.Add, sympy.Mul)):
        t = type(e)
        return t(*[laplace_transform_derivatives(arg) for arg in e.args])
    
    return e

L_ode_2 = laplace_transform_derivatives(L_ode)

L_ode_3 = L_ode_2.subs(L_y, Y)

ics = {y(0): 1, y(t).diff(t).subs(t, 0): 0}

L_ode_4 = L_ode_3.subs(ics)

Y_sol = sympy.solve(L_ode_4, Y)

print ('here2')
y_sol = sympy.inverse_laplace_transform(Y_sol[0], s, t)

y_t = sympy.lambdify(t, y_sol.evalf(), 'numpy')

print ('here1')
fig, ax = plt.subplots(figsize=(8, 4))

tt = np.linspace(0, 10, 500)
ax.plot(tt, y_t(tt).real)
ax.set_xlabel(r"$t$", fontsize=18)
ax.set_ylabel(r"$y(t)$", fontsize=18)
fig.tight_layout()
print ('here')
plt.show()
