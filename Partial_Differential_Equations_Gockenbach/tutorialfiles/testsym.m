clear
syms r
l=solve(r^2+4*r-3,r)
syms t c1 c2 u
u=c1*exp(l(1)*t)+c2*exp(l(2)*t)
c=solve(subs(u,t,0)-1,subs(diff(u,t),t,0),c1,c2)
u=subs(u,{c1,c2},{c.c1,c.c2})
