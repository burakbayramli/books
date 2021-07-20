import sympy as sym
x, L, C, D, c_0, c_1, = sym.symbols('x L C D c_0 c_1')

def model1(f, L, D):
    """Solve -u'' = f(x), u(0)=0, u(L)=D."""
    # Integrate twice
    u_x = - sym.integrate(f, (x, 0, x)) + c_0
    u = sym.integrate(u_x, (x, 0, x)) + c_1
    # Set up 2 equations from the 2 boundary conditions and solve
    # with respect to the integration constants c_0, c_1
    r = sym.solve([u.subs(x, 0)-0,  # x=0 condition
                   u.subs(x,L)-D],  # x=L condition
                  [c_0, c_1])       # unknowns
    # Substitute the integration constants in the solution
    u = u.subs(c_0, r[c_0]).subs(c_1, r[c_1])
    u = sym.simplify(sym.expand(u))
    return u

def model2(f, L, C, D):
    """Solve -u'' = f(x), u'(0)=C, u(L)=D."""
    u_x = - sym.integrate(f, (x, 0, x)) + c_0
    u = sym.integrate(u_x, (x, 0, x)) + c_1
    r = sym.solve([sym.diff(u,x).subs(x, 0)-C,  # x=0 cond.
                   u.subs(x,L)-D],              # x=L cond.
                  [c_0, c_1])
    u = u.subs(c_0, r[c_0]).subs(c_1, r[c_1])
    u = sym.simplify(sym.expand(u))
    return u

def model3(f, a, L, C, D):
    """Solve -(a*u')' = f(x), u(0)=C, u(L)=D."""
    au_x = - sym.integrate(f, (x, 0, x)) + c_0
    u = sym.integrate(au_x/a, (x, 0, x)) + c_1
    r = sym.solve([u.subs(x, 0)-C,
                   u.subs(x,L)-D],
                  [c_0, c_1])
    u = u.subs(c_0, r[c_0]).subs(c_1, r[c_1])
    u = sym.simplify(sym.expand(u))
    return u


def demo():
    f = 2
    u = model1(f, L, D)
    print('model1:', u, u.subs(x, 0), u.subs(x, L))
    print(sym.latex(u, mode='plain'))
    u = model2(f, L, C, D)
    #f = x
    #u = model2(f, L, C, D)
    print('model2:', u, sym.diff(u, x).subs(x, 0), u.subs(x, L))
    print(sym.latex(u, mode='plain'))
    u = model3(0, 1+x**2, L, C, D)
    print('model3:', u, u.subs(x, 0), u.subs(x, L))
    print(sym.latex(u, mode='plain'))

if __name__ == '__main__':
    demo()


