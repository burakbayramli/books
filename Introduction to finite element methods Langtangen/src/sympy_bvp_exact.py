from sympy import *
i, b_L, b_0, h, N = symbols('i b_L b_0 h N')
L = N*h
x_i = i*h
u_i = -x_i**2 + (b_L + 2*L)*x_i + b_0
u_im1 = u_i.subs(i, i-1)
u_ip1 = u_i.subs(i, i+1)

# General equation
R = 1/h**2*(-u_im1 + 2*u_i - u_ip1) - 2
print(R)
R = simplify(R)
print(R)

# Right boundary equation
R = 1/h**2*(-u_im1 + u_i) - b_L/h - 1
R = R.subs(i, N)
print(R)
R = simplify(R)
print(R)
