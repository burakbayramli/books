# sympy_examples.py
# -------------------------------------------------------------------------
# Demonstrate some useful methods available in the SymPy module.
# ------------------------------------------------------------------------- 
from sympy import *
init_session()

expand( (x + y)**5 )
factor( x**6 - 1 )
pi.n(100)
plot(besselj(0, x), besselj(1, x), (x, 0, 10))
diff( x*sin(y), x, y )
integrate(cos(x)**2, x)
integrate(exp(-x**2), (x, -oo, oo))
Sum(k**3, (k, 0, m)).doit().factor()
dsolve(f(x).diff(x) + f(x) - x, f(x)).simplify()
