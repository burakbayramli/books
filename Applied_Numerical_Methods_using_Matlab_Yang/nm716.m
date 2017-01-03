%nm716  to minimize f(x) by the conjugate gradient method.
f713=inline('x(1).^2-4*x(1)-x(1).*x(2)+x(2).^2-x(2)','x');  
x0=[0 0], TolX= 1e-4; TolFun= 1e-4; alpha0=10; MaxIter=100;
[xo,fo]= opt_conjg(f713,x0,TolX,TolFun,alpha0,MaxIter,1)
[xo,fo]= opt_conjg(f713,x0,TolX,TolFun,alpha0,MaxIter,2)
