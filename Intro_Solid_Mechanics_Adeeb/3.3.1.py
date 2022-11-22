import sympy as sp
from sympy import diff, simplify
from sympy.matrices import Matrix
sp.init_printing(use_latex = "mathjax")
x1,x2 = sp.symbols('x_1 x_2')
r, theta = sp.symbols('r \u03B8')
u1 = x1**2+x2**2
u2 = x1*x2
# cartesian coordinate system
u = Matrix([u1,u2])
print("u =",u)
x=Matrix([x1,x2])
# gradient
grad_u=Matrix([[diff(ui,xj) for xj in x] for ui in u])
print("\u2207u =",grad_u)
# Change of variables
print("The following is a simple change of variables but the components are still in the cartesian coordinate system")
urth = u.subs([(x1, r*sp.cos(theta)), (x2, r*sp.sin(theta))])
print("u_r_\u03B8 =",urth,simplify(urth))
# gradient
grad_u_rth = grad_u.subs([(x1, r*sp.cos(theta)), (x2, r*sp.sin(theta))])
print("\u2207u_r_\u03B8 =",grad_u_rth)
# rot matrix
Q = Matrix([[sp.cos(theta),sp.sin(theta)],[-sp.sin(theta),sp.cos(theta)]])
print("Q =",Q)
# Change of coordinates
print("The following representation is in the radial coordinate system")
upolar=simplify(Q*urth)
grad_u_polar=simplify(Q*grad_u_rth*Q.T)
print("u' = Q*u =",upolar)
print("\u2207u' = Q*\u2207u*Q^T =",grad_u_polar)
# Gradient formula in the polar coordinate system
print("The gradient can also be calculated using the gradient formula")
grad_u_formula=Matrix([[diff(upolar[0],r),diff(upolar[0],theta)/r-upolar[1]/r],
                [diff(upolar[1],r),upolar[0]/r+diff(upolar[1],theta)/r]])
grad_u_formula=simplify(grad_u_formula)
print(grad_u_formula)
