function sol = ch3ex1
solinit = bvpinit(linspace(0,1,5),@guess);
sol = bvp4c(@odes,@bcs,solinit);
plot(sol.x,sol.y(1,:));
%print -depsc ch3fig1a
figure
xint = linspace(0,1,100);
Sxint = deval(sol,xint);
plot(xint,Sxint(1,:));
%print -depsc ch3fig1b
%=================================================
function v = guess(x)
v = [ x*(1-x); 1-2*x ];

function dydx = odes(x,y)
dydx = [ y(2); -exp(y(1)) ];

function res = bcs(ya,yb)
res = [ ya(1); yb(1) ];