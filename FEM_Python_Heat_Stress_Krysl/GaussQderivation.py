# Finite Element Modeling with Abaqus and Python for Thermal and 
# Stress Analysis
# (C)  2017, Petr Krysl
#
"""
 Derivation of the Gauss quadrature rule.
 Evaluated symbolically.
 """

import sympy


xi, xi1, xi2 = sympy.symbols('xi, xi1, xi2')
I1 = sympy.integrate((xi-xi1)*(xi-xi2), xi)
I2 = sympy.integrate(xi*(xi-xi1)*(xi-xi2), xi)
I1v = I1.subs(xi, +1) - I1.subs(xi, -1)
I2v = I2.subs(xi, +1) - I2.subs(xi, -1)

solution = sympy.solve([I1v, I2v], xi1, xi2)
print(solution)

