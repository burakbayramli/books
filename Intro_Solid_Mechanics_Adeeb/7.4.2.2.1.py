from sympy import sqrt,Matrix,simplify
import sympy as sp
a1,a2,a3 = sp.symbols("a_1 a_2 a_3")
Q = Matrix([[1/sqrt(3),1/sqrt(3),1/sqrt(3)],
   [1/sqrt(2),-1/sqrt(2),0],
   [1/sqrt(6),1/sqrt(6),-2/sqrt(6)]])
a = Matrix([a1, a2, a3])
s = Q.T*a;
display("\u03C3 =",s)
display("2*\u03C3_y=")
simplify((s[0]-s[1])**2+(s[1]-s[2])**2+(s[0]-s[2])**2)
