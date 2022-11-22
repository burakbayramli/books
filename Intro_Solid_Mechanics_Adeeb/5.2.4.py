from sympy import Matrix,simplify,diff,solve
import sympy as sp
s1,s2,t = sp.symbols("sigma_1 sigma_2 theta")
ss = Matrix([[s1,0],[0,s2]])
display("\u03C3 =",ss)
Q = Matrix([[sp.cos(t), -sp.sin(t)], 
            [sp.sin(t), sp.cos(t)]])
display("Q =",Q)
news = simplify(Q*ss*Q.T)
display("Stress Matrix")
display("\u03C3' =",news)
eq1 = diff(news[0,1],t)
display("Max value of \u03C3'_12 =",eq1)
s = solve(eq1,t)
display("\u03B8 =",s,"degrees: ",[i*180/sp.pi for i in s])
