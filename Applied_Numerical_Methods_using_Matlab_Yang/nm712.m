%nm712.m  to perform the quadratic approximation method
clear, clf
f711=inline('(x.*x-4).^2/8-1', 'x');
a=0; b=3; TolX=1e-5; TolFun=1e-8; MaxIter=100;
[xoq,foq]=opt_quad(f711,[a b],TolX,TolFun,MaxIter) 
%minimum point and its function value
[xob,fob]=fminbnd(f711,a,b) %MATLAB built-in function
