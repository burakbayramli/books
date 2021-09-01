# Operations with polynomials
from elemfunc import *

n = 3                                                  # degree of polynomial
a = [6e0, -11e0, 6e0, 3e0]                                     # coefficients
b = [0]*(n+1)                                                    # work array
x0 = 1e0

print("Coefficients of polynomial:")
print(a)

print("\nPolynomial value:")                       # evaluation of polynomial
print("P(",x0,") =",Poly(x0, a, n))

PolyDerive(a, b, n)                                # derivative of polynomial
print("\nCoefficients of derivative:")
print(b)

PolyDivide(x0, a, b, n)                        # synthetic division by (x-x0)
print("\nCoefficients of quotient and remainder:")
print(b)
R = b[n]                                                         # remainder
print("\nCheck of remainder:")
print("R - P(x0) =",R-Poly(x0,a,n))

x1 = 2e0                                    # check synthetic division for x1
P = Poly(x1,a,n)                                           # polynomial value
Q = Poly(x1,b,n-1)                                           # quotient value
print("\nCheck of division for x1 =",x1,":")
print("P(x1) - (x1-x0)Q(x1) - R =",P - (x1-x0)*Q - R)
