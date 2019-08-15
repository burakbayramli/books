function test02
%Exact example, check for pressure condition
syms x y 
p = 2*pi*cos(pi*x)*sin(pi*y);
intxp = int(p,x,0,1);
intp = int(intxp,y,-0.5,0.5)