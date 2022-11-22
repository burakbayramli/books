from sympy import dsolve,Eq,trigsimp
import sympy as sp
m,g,k,t = sp.symbols("m g k t")
x = sp.Function("x")
dx = x(t).diff(t)
ddx = x(t).diff(t,2)
x1 = x(t).subs(t,0)
dx1 = dx.subs(t,0)
s = trigsimp(dsolve(m*g-k*x(t)-m*ddx,x(t),ics ={x1:0,dx1:0}))
display(s)
display(s.rewrite(sp.cos).simplify())
display("Note that for t,k and m > 0 the following two expressions are equal")
gg = Eq(sp.cosh(t*sp.sqrt((-k/m))),sp.cos(t*sp.sqrt((k/m))))
display(gg)
