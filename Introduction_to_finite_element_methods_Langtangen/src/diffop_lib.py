# https://github.com/sympy/sympy/wiki/Generating-tables-of-derivatives-and-integrals

from sympy import *
t, dt, n, w = symbols('t dt n w', real=True)

# Finite difference operators

def D_t_forward(u):
    return (u(t + dt) - u(t))/dt

def D_t_backward(u):
    return (u(t) - u(t-dt))/dt

def D_t_centered(u):
    return (u(t + dt/2) - u(t-dt/2))/dt

def D_2t_centered(u):
    return (u(t + dt) - u(t-dt))/(2*dt)

def D_t_D_t(u):
    return (u(t + dt) - 2*u(t) + u(t-dt))/(dt**2)


op_list = [D_t_forward, D_t_backward,
           D_t_centered, D_2t_centered, D_t_D_t]

def ft1(t):
    return t

def ft2(t):
    return t**2

def ft3(t):
    return t**3

def f_expiwt(t):
    return exp(I*w*t)

def f_expwt(t):
    return exp(w*t)

func_list = [ft1, ft2, ft3, f_expiwt, f_expwt]
import inspect

for func in func_list:
    print('\n--- Function:', inspect.getsource(func), '---')
    for op in op_list:
        print('\nOperator:', op.__name__)
        f = func
        e = op(f)
        e = simplify(expand(e))
        print('simplify(expand(operator(function)):', e)
        if func in [f_expiwt, f_expwt]:
            e = e/f(t)
        e = e.subs(t, n*dt)
        print('t -> n*dt:', expand(e))
        print('factor(simplify(expand(e))):', factor(simplify(expand(e))))
