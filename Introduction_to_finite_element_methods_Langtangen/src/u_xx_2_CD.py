from sympy import *
x, C, D = symbols('x C D')
i, j = symbols('i j', integer=True, positive=True)
psi_i = (1-x)**(i+1)
psi_j = psi_i.subs(i, j)
integrand = diff(psi_i, x)*diff(psi_j, x)
integrand = simplify(integrand)
A_ij = integrate(integrand, (x, 0, 1))
A_ij = simplify(A_ij)
print('A_ij:', A_ij)
f = 2
b_i = integrate(f*psi_i, (x, 0, 1)) - \
      integrate(diff(D*x, x)*diff(psi_i, x), (x, 0, 1)) - \
      C*psi_i.subs(x, 0)
b_i = simplify(b_i)
print('b_i:', b_i)
N = 1
A = zeros(N+1, N+1)
b = zeros(N+1)
print('fresh b:', b)
for r in range(N+1):
    for s in range(N+1):
        A[r,s] = A_ij.subs(i, r).subs(j, s)
    b[r,0] = b_i.subs(i, r)
print('A:', A)
print('b:', b[:,0])
c = A.LUsolve(b)
print('c:', c[:,0])
u = sum(c[r,0]*psi_i.subs(i, r) for r in range(N+1)) + D*x
print('u:', simplify(u))
print("u'':", simplify(diff(u, x, x)))
print('BC x=0:', simplify(diff(u, x).subs(x, 0)))
print('BC x=1:', simplify(u.subs(x, 1)))
