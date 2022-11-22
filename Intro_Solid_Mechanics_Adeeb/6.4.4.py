from sympy import diff,Eq,solve,Matrix
import sympy as sp
sp.init_printing(use_latex="mathjax")
x1,x2,x3 = sp.symbols("x1 x2 x3")
a,b,c,d = sp.symbols("\u03B1 \u03B2 \u03B3 \u03B4")
x = Matrix([x1,x2,x3])
s = Matrix([[a*x1**2+d*x1+3*x2+x3,-b*x2*x1,0],
            [-b*x2*x1, 5*x2**2,0],
            [0,0,c*x3**2]])
display("stress field inside a body in static equilibrium")
display("\u03C3 =", s)
pb = Matrix([x1, x2, x3])*10
display("body force vector")
display("pb =", pb)
# systems of equation:
# d/dx_1[x_11]+d/dx_2[x_21]+d/dx_3[x_31]+pb_1=0
# d/dx_1[x_12]+d/dx_2[x_22]+d/dx_3[x_32]+pb_2=0
# d/dx_1[x_13]+d/dx_2[x_23]+d/dx_3[x_33]+pb_3=0
equation = [sum([diff(s[i,j], x[i])for i in range(3)])+pb[j] for j in range(3)]
display("system of equations =",Eq(equation[0],0),Eq(equation[1],0),Eq(equation[2],0))
Eq0 = Eq(d,0)
Eq1 = Eq(equation[0].coeff(x1),0)
Eq2 = Eq(equation[1].coeff(x2),0)
Eq3 = Eq(equation[2].coeff(x3),0)
display("because these equations are valid for all values of x1, x2, and x3. They can be refined to =",Eq0,Eq1,Eq2,Eq3)
equations=[d,equation[0].coeff(x1),equation[1].coeff(x2),equation[2].coeff(x3)]
sol = solve(equations,[a,b,c,d])
display("Solution:")
display("\u03B1 =", sol[a])
display("\u03B2 =", sol[b])
display("\u03B3 =", sol[c])
display("\u03B4 =", sol[d])
