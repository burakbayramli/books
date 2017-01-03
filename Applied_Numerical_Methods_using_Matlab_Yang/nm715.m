%nm715 to minimize an objective ftn f(x) by the Newton method.
clear, clf
f713=inline('x(1).^2-4*x(1)-x(1).*x(2)+x(2).^2-x(2)','x');  
g713=inline('[2*x(1)-x(2)-4 2*x(2)-x(1)-1]','x');  
x0=[0 0],  TolX=1e-4; TolFun=1e-6;  MaxIter= 50;
[xo,go,xx]=newtons(g713,x0,TolX,MaxIter);
xo, f713(xo) %an extremum point reached and its function value
